#!/usr/bin/env python3
"""Render benchmark results (bench/results/TAG-{sbcl,ccl}.tsv) as markdown.

  report.py TAG                 full tables for TAG
  report.py TAG --before TAG0   also a before/after table of the vm timings
  report.py TAG --brief         only the summary tables (n = 4096 and 10^6)
Output goes to stdout.  Times are ns per element (array section) or per call (scalar section),
the best of several repetitions, taken from the .tsv files written by bench.lisp.
"""
import sys, os, argparse, collections

HERE = os.path.dirname(os.path.abspath(__file__))
SIZES = [1, 16, 256, 4096, 65536, 1000000, 10000000]
IMPLS = ["ccl", "sbcl"]

def load(tag):
    data = {}   # impl -> {(section, family, op, type, mode): {n: ns}}
    meta = {}
    for impl in IMPLS:
        p = os.path.join(HERE, "results", f"{tag}-{impl}.tsv")
        if not os.path.exists(p):
            continue
        d = collections.OrderedDict()
        for line in open(p):
            f = line.rstrip("\n").split("\t")
            if f[0] == "#":
                meta.setdefault(impl, []).append(" ".join(f[1:]))
                continue
            if len(f) < 8:
                continue
            _, section, family, op, typ, n, mode, ns = f[:8]
            d.setdefault((section, family, op, typ, mode), {})[int(n)] = float(ns)
        data[impl] = d
    return data, meta

def fmt(v):
    if v is None:
        return ""
    if v >= 1000: return f"{v:,.0f}"
    if v >= 100: return f"{v:.0f}"
    if v >= 10: return f"{v:.1f}"
    return f"{v:.2f}"

def sizes_label(n):
    return {1000000: "1e6", 10000000: "1e7"}.get(n, str(n))

def array_tables(d, impl, sizes):
    out = []
    keys = [k for k in d if k[0] == "array"]
    cases = list(collections.OrderedDict.fromkeys(k[1:4] for k in keys))
    fams = list(collections.OrderedDict.fromkeys(c[0] for c in cases))
    for fam in fams:
        out.append(f"\n#### {impl}: {fam} (ns per element)\n")
        out.append("| op | type | impl | " + " | ".join(sizes_label(n) for n in sizes) + " |")
        out.append("|---|---|---|" + "---:|" * len(sizes))
        for c in cases:
            if c[0] != fam: continue
            for mode in ["base", "vm", "vm-lisp", "vm-mkl"]:
                r = d.get(("array",) + c + (mode,))
                if r is None: continue
                label = {"base": "`cl:` loop", "vm": "vm", "vm-lisp": "vm, Lisp kernel", "vm-mkl": "vm, MKL forced"}[mode]
                out.append(f"| {c[1]} | {c[2]} | {label} | " + " | ".join(fmt(r.get(n)) for n in sizes) + " |")
    return out

def ratio_table(d, impl, sizes):
    out = [f"\n#### {impl}: vm / `cl:` loop (time ratio; below 1 = vm faster)\n",
           "| family | op | type | " + " | ".join(sizes_label(n) for n in sizes) + " |",
           "|---|---|---|" + "---:|" * len(sizes)]
    cases = list(collections.OrderedDict.fromkeys(k[1:4] for k in d if k[0] == "array"))
    for c in cases:
        b = d.get(("array",) + c + ("base",)); v = d.get(("array",) + c + ("vm",))
        if not b or not v: continue
        cells = []
        for n in sizes:
            cells.append(f"{v[n]/b[n]:.2f}" if n in b and n in v and b[n] > 0 else "")
        out.append(f"| {c[0]} | {c[1]} | {c[2]} | " + " | ".join(cells) + " |")
    return out

def scalar_table(d, impl):
    out = [f"\n#### {impl}: scalar calls, compiled (ns per call)\n",
           "| operation | `cl:` declared | vm declared | `cl:` undeclared | vm undeclared |", "|---|---:|---:|---:|---:|"]
    ops = list(collections.OrderedDict.fromkeys(k[2] for k in d if k[0] == "scalar"))
    for op in ops:
        row = []
        for mode in ["cl-decl", "vm-decl", "cl-undecl", "vm-undecl"]:
            r = d.get(("scalar", "scalar", op, "-", mode))
            row.append(fmt(r[1]) if r else "")
        out.append(f"| {op} | " + " | ".join(row) + " |")
    return out

def before_after(db, da, impl, sizes):
    out = [f"\n#### {impl}: vm timings before and after tuning (ns per element)\n",
           "| family | op | type | " + " | ".join(f"{sizes_label(n)} before | after" for n in sizes) + " |",
           "|---|---|---|" + "---:|---:|" * len(sizes)]
    for k in da:
        if k[0] != "array" or k[4] != "vm" or k not in db: continue
        changed = any(n in db[k] and abs(da[k][n] - db[k][n]) > 0.25 * db[k][n] for n in sizes if n in da[k])
        if not changed: continue
        cells = []
        for n in sizes:
            cells += [fmt(db[k].get(n)), fmt(da[k].get(n))]
        out.append(f"| {k[1]} | {k[2]} | {k[3]} | " + " | ".join(cells) + " |")
    return out

README_CASES = [("arith", "+", "double"), ("arith", "*", "single"), ("arith", "/", "double"), ("arith", "+", "cdouble"),
                ("arith", "+", "sb64"), ("arith", "+", "T"),
                ("transc", "exp", "double"), ("transc", "log", "double"), ("transc", "sin", "single"),
                ("transc", "sqrt", "double"), ("transc", "sqrt", "single"), ("transc", "expt", "double"),
                ("compare", "<", "double"), ("compare", "=", "sb64"), ("compare", "max", "double"),
                ("division", "floor", "double"), ("division", "round", "double"), ("division", "floor", "single"),
                ("division", "floor", "sb64"),
                ("integer", "logand", "sb64"), ("integer", "ash", "sb64"),
                ("convert", "float", "sb64->double"), ("convert", "float", "double->single")]

def readme_tables(data):
    """Compact tables for the README: ns per element at 10^6 and the time of a call on 16 elements."""
    out = []
    for n, title in ((1000000, "10^6 elements"), (16, "16 elements (the cost of the call dominates)")):
        out.append(f"\n**{title}**, nanoseconds {'per element' if n > 16 else 'per call'}\n")
        out.append("| operator | element type | CCL `cl:` loop | CCL vm | CCL vm, no MKL | SBCL `cl:` loop | SBCL vm | SBCL vm, no MKL |")
        out.append("|---|---|---:|---:|---:|---:|---:|---:|")
        for c in README_CASES:
            row = []
            for impl in IMPLS:
                d = data.get(impl, {})
                for mode in ("base", "vm", "vm-lisp"):
                    r = d.get(("array",) + c + (mode,))
                    v = r.get(n) if r else None
                    if v is not None and n == 16: v = v * n
                    row.append(fmt(v) if v is not None else ("" if mode == "vm-lisp" else "n/a"))
            out.append(f"| `{c[1]}` | {c[2]} | " + " | ".join(row) + " |")
    return out

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("tag"); ap.add_argument("--before"); ap.add_argument("--brief", action="store_true")
    ap.add_argument("--readme", action="store_true", help="only the compact tables for the README")
    a = ap.parse_args()
    data, meta = load(a.tag)
    if a.readme:
        print("\n".join(readme_tables(data)))
        return
    sizes = [4096, 1000000] if a.brief else SIZES
    print(f"<!-- generated by bench/report.py from bench/results/{a.tag}-*.tsv -->")
    for impl, m in meta.items():
        print(f"- {impl}: " + "; ".join(m))
    for impl in IMPLS:
        if impl not in data: continue
        d = data[impl]
        print(f"\n### {impl}")
        print("\n".join(ratio_table(d, impl, sizes)))
        if not a.brief:
            print("\n".join(array_tables(d, impl, sizes)))
        print("\n".join(scalar_table(d, impl)))
    if a.before:
        db, _ = load(a.before)
        print("\n### Before / after tuning")
        for impl in IMPLS:
            if impl in db and impl in data:
                print("\n".join(before_after(db[impl], data[impl], impl, [4096, 1000000, 10000000])))

if __name__ == "__main__":
    main()
