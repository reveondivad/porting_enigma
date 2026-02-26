import re
import csv
from collections import Counter

INPUT = "evidence/file_list.txt"
OUT_MANIFEST = "evidence/MANIFEST.csv"
OUT_COUNTS = "stats/COUNTS.txt"

def extract_path(line: str):
    # lines look like: "3 ./enigma.8th" or "5 ./enigma.objc.m"
    m = re.search(r'(\./\S+)$', line.strip())
    return m.group(1) if m else None

def language_id_from_path(path: str):
    # ./enigma.objc.m -> objc.m
    # ./Enigma.cs -> cs
    base = path.split("/")[-1]
    base = base.replace("Enigma.", "enigma.")
    if not base.startswith("enigma"):
        return None
    parts = base.split(".", 1)
    return parts[1] if len(parts) > 1 else None

def normalize(lang_id: str):
    # remove obvious “variant” suffix patterns
    # pony3..pony9 -> pony
    # vale10..vale13 -> vale
    # groovy2 -> groovy
    # zig2 -> zig
    # wasmx -> wasm etc (keep conservative)
    s = lang_id

    # drop leading enigma_2 style duplicates already handled by list name
    s = s.replace("_2.", ".")
    s = s.replace("enigma_2.", "")

    # strip trailing digits for common variant scheme: foo2, foo3, foo10...
    s = re.sub(r'(\D)\d+$', r'\1', s)

    # special-case patterns like vale10 -> vale
    s = re.sub(r'^vale\d+$', 'vale', s)
    s = re.sub(r'^pony\d+$', 'pony', s)

    # keep multi-dot ids like objc.m or pl.prolog as-is
    return s

rows = []
raw_ids = []
norm_ids = []

with open(INPUT, "r", encoding="utf-8") as f:
    for line in f:
        p = extract_path(line)
        if not p:
            continue
        lang_id = language_id_from_path(p)
        if not lang_id:
            continue
        n = normalize(lang_id)
        rows.append((p, lang_id, n))
        raw_ids.append(lang_id)
        norm_ids.append(n)

raw_count = len(set(raw_ids))
norm_count = len(set(norm_ids))
file_count = len(rows)

with open(OUT_MANIFEST, "w", newline="", encoding="utf-8") as f:
    w = csv.writer(f)
    w.writerow(["path", "language_id", "language_normalized"])
    w.writerows(rows)

c_raw = Counter(raw_ids)
c_norm = Counter(norm_ids)

with open(OUT_COUNTS, "w", encoding="utf-8") as f:
    f.write(f"File count (implementations): {file_count}\n")
    f.write(f"Distinct language identifiers (raw): {raw_count}\n")
    f.write(f"Distinct programming languages (normalized): {norm_count}\n\n")
    f.write("Top normalized languages by number of files (variants collapsed):\n")
    for k, v in c_norm.most_common(30):
        f.write(f"{k}: {v}\n")

print("Wrote:", OUT_MANIFEST, OUT_COUNTS)
print("Counts:", file_count, raw_count, norm_count)
