# The combined dataset moved

It is now built and maintained by the corpus chapter that uses it:

    book/labs/03-elections/nomination-anchors/

- `data/raw/` — the five transcribed tables, exactly as read off the printed page
- `data/build-data.R` — joins them, rebuilds the column the book never printed,
  and refuses to write anything unless four derived counts land on four totals
  the book itself prints
- `data/derived/nominations.csv` — the 55-row result

**There is deliberately no second copy here.** An earlier version of this folder
held a Python build and its own CSV. Two builds of one table drift, and then two
places disagree about a number they both computed from the same source. The
chapter's build is the only one.

The reading notes behind the transcription, including all 23 of the book's
tables, are in [../potg-book-notes.md](../potg-book-notes.md). The access and
licensing situation is in [../NOTES.md](../NOTES.md).
