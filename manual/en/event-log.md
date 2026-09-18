# The event log

Every job writes a log while it runs. One file per job, one line per event. The log stays after the
job, and after the application closes.

## Where it is

| System | Folder |
|---|---|
| Linux, macOS | `~/.local/state/mediacopy3000/jobs/` |
| Windows | `%LOCALAPPDATA%\mediacopy3000\jobs\` |

On Linux, the variable `XDG_STATE_HOME` moves the folder.

The file name is the time the job was made, its number, the name of its folder, and its kind:
`2026-09-15_101500-1-CARD_A001-offload.log`. The number after the time is the job's number in that
run of the application. The saved report names the file on its `Log` line.

## What it holds

The file starts with a title line, then the plan you approved, as the report prints it. Then one
line per event, with the time in UTC:

```
2026-09-15T10:15:00.123Z planned 7 files, 18600000000 bytes to read
2026-09-15T10:15:00.201Z A/1.mxf Copying
2026-09-15T10:15:00.302Z progress 4194304
2026-09-15T10:15:04.881Z A/1.mxf Saving to disk
2026-09-15T10:15:05.117Z A/1.mxf Naming the copy
2026-09-15T10:15:05.203Z A/1.mxf Verifying
2026-09-15T10:15:07.910Z A/1.mxf Verified
2026-09-15T10:16:35.114Z writing the manifest
2026-09-15T10:16:40.007Z manifest /Volumes/Shuttle-01/CARD_A001/ascmhl/0002_CARD_A001_2026-09-15_101640.mhl
2026-09-15T10:16:40.010Z finished, all ok
```

The file states are the states of the file list. The page [While a job runs](running-a-job.md)
lists them. A `progress` line comes about ten times a second while the job copies.

## When to send it

A problem report is most useful with the saved report and this log. Together they hold the plan,
the result, and the order in which things happened.
