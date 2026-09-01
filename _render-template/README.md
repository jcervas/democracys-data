# A copy of the shared renderer, for the record

These files are **copies**. The originals live in

    Academic/_teaching/_syllabus-template/

and that is what actually renders a brief: every `knit:` field in every
`*-brief.Rmd` points four levels up at the shared copy, and so does
`labs/_lib/check-all.sh`'s caller. Editing anything in this folder changes
nothing.

## Why keep a copy at all

The renderer is not incidental to the corpus. It repairs the base64 data URIs
that pandoc writes in a form browsers refuse to decode — without that pass
every embedded figure in every brief is silently broken — and since this
session it also runs `labs/_lib/check-all.sh` before rendering anything, so a
failed check stops the render. A reader of this repository in a year, holding
the briefs and none of the surrounding directories, would otherwise have no
way to know how the HTML was produced or why it looks repaired.

## It will drift

Nothing keeps this in step with the original. The shared template is shared on
purpose — a change made there is meant to reach every course at once — so the
honest expectation is that this copy is a snapshot of how the corpus was built
at the commit it sits in, not a mirror. If the two disagree, the one in
`_syllabus-template/` is the one that ran.

Copied 12 August 2026, after adding the pre-render check hook.

## Is this copy still true?

A frozen copy that has quietly drifted is worse than no copy: it answers
questions wrongly. `MANIFEST.txt` records the sha256 of each file as of the
snapshot, and `verify.sh` re-checks them against the live renderer:

    sh _render-template/verify.sh

It prints `snapshot matches the live renderer`, or names the files that
drifted. If it reports drift, find out what changed in the shared template
before refreshing the copy — another course may have changed it for reasons
this book needs to know about.

Snapshot refreshed 1 September 2026, after the 3rd-edition rewrite added the
`.dd-fig` block to `brief.css` for the shared chart library.
