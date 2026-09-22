# Reports

A report is a plain text file that records what one job did. It is for the person who must answer,
weeks later, what happened to a media source.

## Save one

1. Select a job that has ended.
2. Click **Save Report…**, or press <kbd>Ctrl</kbd>+<kbd>S</kbd>.
3. Choose where to put the file.

The suggested name is `<folder name>-report.txt`.

The button works only for a job that has finished, failed or was cancelled. A running job has no
result to report.

## What it holds

```
MediaCopy 3000 report
Job: offload
Source: /media/CARD_A001
Destination: /Volumes/Shuttle-01/2026-09-12
Destination: /Volumes/Archive-A/2026-09-12
Created: 2026-09-12T14:03:00+00:00
Plan
  files: 7
  bytes: 17260697327
  originals: the seal this job takes first
  seal first: 7 files, 17260697327 bytes, on failure: stop before copying
  generation: /Volumes/Shuttle-01/2026-09-12 (3, 0003_CARD_A001_2026-09-12_140300.mhl)
  generation: /Volumes/Archive-A/2026-09-12 (1, 0001_CARD_A001_2026-09-12_140300.mhl)
  target: /Volumes/Shuttle-01/2026-09-12 (empty)
  target: /Volumes/Archive-A/2026-09-12 (will be created)
  warning: folder is already sealed – generation 2
Result: finished, all files verified
Files: 7 total, 7 verified, 0 hash mismatch/io error, 0 missing, 0 new, 0 replaced
Manifest: /Volumes/Shuttle-01/2026-09-12/CARD_A001/ascmhl/0003_CARD_A001_2026-09-12_140300.mhl
Manifest: /Volumes/Archive-A/2026-09-12/CARD_A001/ascmhl/0001_CARD_A001_2026-09-12_140300.mhl
Originals: the media source's own history, 7 files
Log: /home/you/.local/state/mediacopy3000/jobs/2026-09-12_140300-1-CARD_A001-offload.log
```

| Section | What it is for |
|---|---|
| `Job`, `Source`, `Destination`, `Folder` | Which job, and which folders |
| `Created` | When the job was made |
| `Plan` | The plan you approved, with its findings, word for word. An offload into a partial destination adds `existing copy: resume` or `replace`. |
| `Result` | How the job ended |
| `Files` | The counters from the detail pane |
| `Manifest` | Every manifest the job wrote |
| `Originals` | What the job checked against |
| `Log` | The event log of the job. The page [The event log](event-log.md) explains it. |

A job with failures gets a `Failures:` section. It has one line for each bad file:

```
Failures:
FAILED  A001C002_260912_R1AB.mov  hash mismatch expected xxh64 4f9a1c3b2d7e8051 actual xxh64 8c21b40fa9e3d517
FAILED  A001C004_260912_R1AB.mov  io error input/output error
MISSING Sidecar/A001C001.wav
```

A verify job or a seal job also gets a `History:` section, with one line for each generation.

## Why the plan is in there

The `Plan` block is the part that makes the report an audit trail. It shows what the application
said before you approved it, including every warning you decided to accept. The report and the
plan sheet cannot disagree, because they read the same plan.

## The report is not the record

The manifest is the record. It lives with the files and outlives this application. The report is a
note about one run.
