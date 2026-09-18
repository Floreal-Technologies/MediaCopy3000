# The ASC MHL format

ASC MHL is the Media Hash List format of the American Society of Cinematographers. MediaCopy 3000
reads and writes version 2.0 of it. This page covers the part of the format you meet in this
application. The specification is the authority.

## The folder layout

Every destination gets a copy of the media source folder. Inside that copy sits an `ascmhl` folder:

```
<destination>/CARD_A001/…the files…
<destination>/CARD_A001/ascmhl/ascmhl_chain.xml
<destination>/CARD_A001/ascmhl/0001_CARD_A001_2026-09-12_140300.mhl
<destination>/CARD_A001/ascmhl/0002_CARD_A001_2026-09-13_091500.mhl
```

The manifest file name holds the generation number, the folder name, the date and the time.

The `ascmhl` folder came with the copy. Its generations are the media source's, and the last one is
the offload's own.

## The chain

`ascmhl_chain.xml` names every generation in order. For each one it holds a `c4` hash of the
manifest file itself.

The `c4` format is fixed by the specification for this one job, so it is never chosen and never
varies, whatever format the files are hashed in.

## A manifest

One manifest describes the tree it sits in. For each file it records the path, the size, the hash,
and what the job did with the file:

| Action | What it means |
|---|---|
| `original` | These are the first hashes anybody took of this file. |
| `verified` | The hash matched a record that already existed. |
| `failed` | The hash did not match the record. |

MediaCopy 3000 writes these three and no others. Its reader also accepts `new`, which older tools
wrote, and reads it as `verified`.

A manifest also records the directory hashes: a content hash and a structure hash for every
directory, and one pair for the root. The specification's Appendix G says how they are rolled up from
the children. The root pair is the `roothash` of the tree.

The entries are in the order the specification gives in section 6.5. For each directory, the
manifest records its subdirectories, then its own files, then the directory itself. The root has no
entry of its own.

## The process of a generation

Every generation says what was done:

| Process | Written by |
|---|---|
| `in-place` | A seal, and a verify |
| `transfer` | The destination side of an offload |
| `flatten` | Not written by this application |

The format has no `verify` process. This is why a verify generation records `in-place`.

## Hash formats

| Format | MediaCopy 3000 |
|---|---|
| `xxh64` | Writes it for every new job, and reads it |
| `md5` | Reads it, and keeps writing it for a folder already recorded in it |
| `sha1` | Reads it, and keeps writing it for a folder already recorded in it |
| `c4` | Used for the chain only |

One job uses one format for every file it hashes. The format is taken from the records the job must
check against, never chosen by hand. When the records hold more than one format, the job stops with
the blocker `hash format cannot be settled`.

## Reading a manifest without this application

The files are XML, and they are meant to outlive any one tool. Other ASC MHL tools read what
MediaCopy 3000 writes. The conformance test suite in this repository checks that against the ASC
reference tool, `ascmhl`.
