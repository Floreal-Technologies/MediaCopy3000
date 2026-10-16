---
layout: home

hero:
  name: MediaCopy 3000
  text: Copy your media and prove the copy
  tagline: Offload camera cards and drives to more than one destination. Each copy is checked and recorded in the ASC MHL format.
  actions:
    - theme: brand
      text: Introduction
      link: /introduction
    - theme: alt
      text: Offload a media source
      link: /offload

features:
  - title: Integrity and completeness
    details: Each job hashes every file and reads each copy back. A manifest proves that the bytes did not change and that every file is there.
  - title: The plan comes first
    details: Before a job starts, the plan shows what the job will do. Blockers and warnings show before a file is copied.
  - title: An industry standard
    details: MediaCopy 3000 reads and writes ASC MHL. Each folder keeps its history as a chain of generations.
---

![The application with a plan ready](images/plan-ready.png)

![The application with all jobs finished](images/job-finished.png)

## Glossary

These are the terms used throughout the manual and the codebase:

<dl>
  <dt>Media source</dt>
  <dd>The source a job reads: a camera card, an SSD, or any folder. Only a seal writes
  to it, and only in its <code>ascmhl</code> folder.
  </dd>
  <dt>Destination</dt>
  <dd>A folder an offload copies into.</dd>
  <dt>Manifest</dt>
  <dd>One <code>.mhl</code> file. It holds a hash for every file in the tree it sits in.</dd>
  <dt>Generation</dt>
  <dd>A manifest in a folder's history, numbered from 1.</dd>
  <dt>Chain</dt>
  <dd>The file that names every generation in order, and holds a hash of each one.</dd>
  <dt>Seal</dt>
  <dd>The recording of a media source's hashes in place, before anything is copied off it.</dd>
  <dt>Offload</dt>
  <dd>
    The copying of a media source to one or more destinations, and checking of each copy.
    Each destination carries the media source's history.
  </dd>
  <dt>Verify</dt>
  <dd>Reading a folder again and checking it against its own history.</dd>
  <dt>Plan</dt>
  <dd>What a job will do, computed before the job runs.</dd>
  <dt>Finding</dt>
  <dd>Something the plan learned that you must know. A blocker or a warning.</dd>
</dl>
