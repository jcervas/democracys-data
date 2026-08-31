# Collecting the class's ratings

The chapter's second map averages the class. This is how the codes get from a
room of students into `derived/class_ratings.csv`.

## Why the form asks for one string instead of 35 questions

A Google Form with 35 seven-way questions is a five-minute form, and the
responses arrive as 35 columns whose headers are prose. The rating map in the
brief already holds every answer, so the form only has to carry the answer out
of the browser. It asks for a name and a 35-character code.

The code is one digit per race, alphabetical by postal abbreviation, `1` = Safe
D through `7` = Safe R. `CODE_ORDER` at the top of `build-data.R` is the
authority on the order, and the same constant is what the brief hands to the
figure — the two cannot drift apart without the build failing.

A 35-question form is still the right answer if you want students who never
open the brief to be able to answer. Nothing below stops you building one; the
build reads any CSV column that contains a decodable code, so a form that
collects 35 separate answers just needs its own small step to join them into
one string first.

## Building the form

Two ways. By hand: create a Google Form with a short-answer question titled
"Your 35-digit code" and a short-answer question for the name, and turn on
response validation (regular expression, matches, `^[1-7]{35}$`) so a mistyped
code is refused at the door rather than dropped silently by this build.

Or paste this into **Extensions ▸ Apps Script** on any Google Sheet, run
`makeSenateForm()`, and read the two URLs it logs:

```js
function makeSenateForm() {
  var form = FormApp.create('84-355 — 2026 Senate ratings');
  form.setDescription(
    'Rate all 35 Senate races in the brief, copy the code it gives you, and ' +
    'paste it below. One digit per race, 1 = Safe D through 7 = Safe R.');
  form.addTextItem().setTitle('Your name').setRequired(true);
  var code = form.addTextItem().setTitle('Your 35-digit code').setRequired(true);
  code.setValidation(FormApp.createTextValidation()
    .setHelpText('35 digits, each from 1 to 7. Copy it from the brief.')
    .requireTextMatchesPattern('^[1-7]{35}$')
    .build());
  var ss = SpreadsheetApp.create('84-355 — 2026 Senate ratings (responses)');
  form.setDestination(FormApp.DestinationType.SPREADSHEET, ss.getId());
  Logger.log('form: %s', form.getPublishedUrl());
  Logger.log('sheet: %s', ss.getUrl());
}
```

## Getting the responses into the build

The responses sheet needs one address this build can read. In the sheet:
**File ▸ Share ▸ Publish to web**, choose the responses tab, choose
**Comma-separated values (.csv)**, publish, and copy the URL.

Then either export a copy to `raw/class-responses.csv`, which the build picks up
on its own, or point it at the published address:

```
DD_SENATE_CLASS_CSV='https://docs.google.com/…/pub?output=csv' Rscript build-data.R
```

Either way the build finds the column holding the codes by decoding them rather
than by name, so a sheet with a timestamp column, a name column and an email
column needs no configuration. Codes that are not 35 digits of 1–7 are counted
as submissions and dropped from the average, and the run says how many of each.

Re-render the brief afterwards and Figure 2 opens on the class layer.

## Doing it live instead

Figure 2 has a **paste the class codes** box. Paste the code column straight out
of the responses sheet, one per line, and the class layer is computed in the
browser. Nothing is rebuilt and nothing is written down, which is the right tool
for the ten minutes in the room and the wrong one for a copy that has to survive
being emailed.

## A privacy note

The responses sheet has names in it. `derived/class_ratings.csv` does not: it
holds one row per race with the mean, the spread and the three counts, and the
individual codes never leave `raw/`. Keep it that way if the chapter is ever
shared outside the class.
