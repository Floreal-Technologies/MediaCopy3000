# Limits

This page says what this version does not do, and what a clean result does not prove. Read it before
you rely on the application for something that matters.

## What a clean verify does not prove

A clean verification proves that the files agree with the manifests. It does not prove that an
earlier copy was complete. A copy that failed before it read a file has no hash for that file in the
manifest, so a later verify has nothing to miss.

## Long paths on Windows

Windows limits a path to 260 characters in the calls this application makes about a file. A media
source with deep folders and long file names can pass that limit, and the file then fails with an
error that names it. Keep the destination near the top of the drive to stay under the limit.

Linux and macOS have no such limit here.

## A volume that refuses an unbuffered read

On Windows, MediaCopy 3000 reads each copy back with the cache turned off. A volume that refuses the
bypass is read through the cache, as before, on every system.

On macOS the bypass is a policy on the reading descriptor. Pages still resident from the write can
answer the read. This version has not been checked on a Mac.

## The chain hash is written, not checked

The chain holds a `c4` hash of every manifest, so a reader can tell whether a manifest was changed
after it was written. MediaCopy 3000 writes that hash and fills it in for older entries. It does not
compare a manifest against it when it reads a history.

## A seal failure can hide behind a good copy

A job that seals before it copies makes two passes over the same files. A file that fails in either
pass counts once in the failure count. The file list shows the last state of each file. So if the
seal cannot read a file and the copy then reads it well, the job reports a failure that the file
list does not show. The seal's manifest has no entry for that file.

## An action this reader does not know becomes `original`

The ASC MHL schema leaves the `action` attribute optional. A manifest that omits it is read as
`original`. A manifest that holds an action word this reader does not know is also read as
`original`, and is written out again as `original`. An unknown action is upgraded rather than
refused.

## Not in this version

| Not yet | What it means for you |
|---|---|
| Devices are not detected | Pick the media source folder by hand. Nothing appears when you insert a card or attach a drive. |
| One hash format per job | A job cannot write two formats at once. |
| One job at a time | Jobs queue. They do not run beside each other. |
| No creator contact fields | The manifest records the computer, the tool and its version, and nothing about a person. |
| The theme is not saved | Every start is back on `Follow desktop`, `System light` and `System dark`. |
| Top-up is not resume | A destination that holds a finished generation of its own is refused. Offload to a fresh folder, or verify the destination. |

## A partial copy from another card

When the media source has no history, the plan cannot tell two cards apart that have the same file
names and sizes. The job finds the difference when it hashes each reused file, and copies the file
again. The file then shows `Replaced after mismatch`.

## Where to report a problem

Report a problem in the project repository. A report is most useful with the saved report of the
job and its event log. The page [The event log](event-log.md) says where the log is.
