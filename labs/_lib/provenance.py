"""provenance.py — make a build script notice when its source moves.

WHY THIS EXISTS

The campaign-visits lab pinned an Associated Press dataset at version /50/ and
captured 323 events. By the time anyone looked again the AP was serving /54/
with 376 events -- the committed file stopped three days before the election
and was missing the entire closing push. Nothing failed. Nothing warned. The
build ran clean and produced stale data.

That is the failure mode this module exists to catch: a URL that still returns
200, still parses, and no longer means what it meant.

WHAT IT DOES

Wrap a download and it records, in `PROVENANCE.tsv` next to the data:

    url, first_seen, last_seen, bytes, sha256, rows

On every later run it compares. If bytes, hash or row count changed it prints a
loud banner saying WHAT changed and BY HOW MUCH, then carries on -- a source
legitimately updating is normal and should not break a build. The point is that
a human sees it.

USE

    import sys; sys.path.insert(0, "../_lib")
    from provenance import fetch, report

    fetch(URL, "raw.csv")          # instead of urllib.request.urlretrieve
    ...
    report()                       # at the end of the script
    stamp()                        # at the end of the script

`fetch` returns the destination path, so it drops into existing code.
"""

import hashlib
import os
import sys
import tempfile
import time
import urllib.parse
import urllib.request
from datetime import date

PROV_FILE = "PROVENANCE.tsv"
COLS = ["url", "first_seen", "last_seen", "bytes", "sha256", "rows"]
_changes = []

# When this module was imported, which is as near as anything gets to when the
# build started. stamp() uses it to tell what the run actually wrote from what
# was already lying in the directory -- see the note above stamp().
START = time.time()

STAMP_FILE = "BUILD-STAMP.tsv"
STAMP_COLS = ["script", "stamped_on", "stamp_source", "file",
              "bytes", "sha256", "rows", "file_mtime"]

_TEXTY = {".csv", ".tsv", ".tab", ".txt", ".json", ".xml"}


def _load():
    if not os.path.exists(PROV_FILE):
        return {}
    out = {}
    with open(PROV_FILE, encoding="utf-8") as fh:
        head = fh.readline().rstrip("\n").split("\t")
        for line in fh:
            v = line.rstrip("\n").split("\t")
            if len(v) == len(head):
                r = dict(zip(head, v))
                out[r["url"]] = r
    return out


def _save(recs):
    with open(PROV_FILE, "w", encoding="utf-8") as fh:
        fh.write("\t".join(COLS) + "\n")
        for r in recs.values():
            fh.write("\t".join(str(r.get(c, "")) for c in COLS) + "\n")


def _sha(path):
    h = hashlib.sha256()
    with open(path, "rb") as fh:
        for chunk in iter(lambda: fh.read(1 << 20), b""):
            h.update(chunk)
    return "sha256:" + h.hexdigest()


def _rows(path):
    if os.path.splitext(path)[1].lower() not in _TEXTY:
        return ""
    try:
        with open(path, "rb") as fh:
            n = sum(1 for _ in fh)
    except OSError:
        return ""
    if os.path.splitext(path)[1].lower() in {".csv", ".tsv", ".tab"}:
        n = max(n - 1, 0)
    return str(n)


def record(url, path, label=None):
    """Record (or compare) one already-downloaded file."""
    recs = _load()
    now = date.today().isoformat()
    new = {"bytes": str(os.path.getsize(path)),
           "sha256": _sha(path), "rows": _rows(path)}
    old = recs.get(url)

    if old is None:
        recs[url] = dict(url=url, first_seen=now, last_seen=now, **new)
        extra = f", {new['rows']} rows" if new["rows"] else ""
        print(f"  [prov] first capture: {os.path.basename(path)} "
              f"({new['bytes']} bytes{extra})")
    else:
        moved = []
        if old.get("bytes") != new["bytes"]:
            moved.append(f"bytes {old.get('bytes')} -> {new['bytes']}")
        if old.get("sha256") != new["sha256"]:
            moved.append("content hash changed")
        if new["rows"] and old.get("rows") != new["rows"]:
            try:
                d = int(new["rows"]) - int(old.get("rows") or 0)
                moved.append(f"rows {old.get('rows')} -> {new['rows']} ({d:+d})")
            except ValueError:
                moved.append(f"rows {old.get('rows')} -> {new['rows']}")
        if moved:
            _changes.append(dict(url=url, file=os.path.basename(path),
                                 first=old.get("first_seen", "?"),
                                 moved=moved, label=label))
        old.update(last_seen=now, **new)
        recs[url] = old
    _save(recs)
    return path


def fetch(url, dest, label=None, timeout=600):
    """Download a URL and record its provenance."""
    urllib.request.urlretrieve(url, dest)
    return record(url, dest, label)


def fetch_bytes(url, label=None, timeout=300, headers=None):
    """Read a URL into memory, recording its provenance on the way.

    For a source the script consumes without ever saving it -- a JSON blob
    parsed straight into a dict, a CSV read into rows and thrown away. Those
    are the easiest fetches to leave unrecorded and no less able to move under
    a build, so the bytes are hashed through a temporary file and the row is
    written exactly as though the file had been kept.

    `headers` matters: several of these sources refuse a request with no user
    agent, and passing them through is the difference between a recorded fetch
    and a 403.
    """
    req = urllib.request.Request(url, headers=headers or {})
    with urllib.request.urlopen(req, timeout=timeout) as r:
        blob = r.read()

    # Name the temp file after the URL rather than taking mkstemp's random
    # name. Two reasons: record() decides whether a row count is meaningful by
    # looking at the extension, and the drift banner prints the basename -- so
    # a random name would report a moved source as "tmpg247f4q8.csv".
    name = os.path.basename(urllib.parse.urlparse(url).path) or "download.bin"
    d = tempfile.mkdtemp()
    tmp = os.path.join(d, name)
    try:
        with open(tmp, "wb") as fh:
            fh.write(blob)
        record(url, tmp, label)
    finally:
        os.unlink(tmp)
        os.rmdir(d)
    return blob


# ---------------------------------------------------------------------------
# THE BUILD STAMP.  The R twin of this file carries the full note; in short:
# PROVENANCE.tsv can only describe a file that was downloaded, which leaves out
# about half the corpus, so BUILD-STAMP.tsv records the smaller thing every
# chapter can say -- which script produced what is in this directory, when it
# last ran, and what the files looked like when it finished.
#
#     script  stamped_on  stamp_source  file  bytes  sha256  rows  file_mtime
#
# stamp_source is "build" when a running script wrote the row and stamped_on is
# a real run date, "disk" when the row was read off the directory afterwards
# and the build date is not known. Only files this run wrote are restamped.
# ---------------------------------------------------------------------------


def _stamp_load():
    if not os.path.exists(STAMP_FILE):
        return {}
    out = {}
    with open(STAMP_FILE, encoding="utf-8") as fh:
        head = fh.readline().rstrip("\n").split("\t")
        for line in fh:
            v = line.rstrip("\n").split("\t")
            if len(v) == len(head):
                r = dict(zip(head, v))
                out[r["file"]] = r
    return out


def _stamp_save(recs):
    with open(STAMP_FILE, "w", encoding="utf-8") as fh:
        fh.write("\t".join(STAMP_COLS) + "\n")
        for k in sorted(recs):
            fh.write("\t".join(str(recs[k].get(c, "")) for c in STAMP_COLS) + "\n")


def _stamp_outputs():
    """Everything a build is allowed to have produced: derived/ and raw/."""
    out = []
    for d in ("derived", "raw"):
        if not os.path.isdir(d):
            continue
        for root, _, files in os.walk(d):
            out.extend(os.path.join(root, f) for f in files)
    return sorted(out)


def stamp(outputs=None, script=None, source_kind="build"):
    """Record which script built what is in this directory, and when."""
    script = script or os.path.basename(sys.argv[0] or "") or ""
    all_out = _stamp_outputs()
    if outputs == "all":
        outputs = all_out
    elif outputs is None:
        outputs = [f for f in all_out if os.path.getmtime(f) >= START]

    recs = _stamp_load()
    # Drop rows for files that are gone. A stamp still listing a deleted file
    # is worse than no stamp: it reads as a promise.
    recs = {k: v for k, v in recs.items() if k in set(all_out)}

    if not outputs:
        _stamp_save(recs)
        print(f"  [stamp] no new outputs this run; {len(recs)} row(s) kept")
        return recs

    now = date.today().isoformat()
    for f in outputs:
        st = os.stat(f)
        recs[f] = {
            "script": script,
            "stamped_on": now,
            "stamp_source": source_kind,
            "file": f,
            "bytes": str(st.st_size),
            "sha256": _sha(f),
            "rows": _rows(f),
            "file_mtime": date.fromtimestamp(st.st_mtime).isoformat(),
        }
    _stamp_save(recs)
    verb = "built on" if source_kind == "build" else "read off disk on"
    print(f"  [stamp] {script}: {len(outputs)} file(s) {verb} {now}")
    return recs


def report():
    """Print the drift banner. Call once at the end of a build script."""
    if not _changes:
        print("  [prov] all sources unchanged since last build")
        return False
    bar = "=" * 72
    print("\n" + bar)
    print("  *** SOURCE DATA CHANGED SINCE THE LAST BUILD ***\n")
    for c in _changes:
        print(f"  {c['file']}\n    {c['url']}\n    first captured {c['first']}")
        for m in c["moved"]:
            print(f"      - {m}")
        print()
    print("  Every figure derived from these files may have moved. Re-check any")
    print("  number quoted in the lab, the key or the brief before trusting it.")
    print(bar + "\n")
    return True
