# Introduction

MediaCopy 3000 copies media and records what it copied. It is made for both
digital imaging technicians (DITs) on a set, and for hobbyists who want to
transfer media in a safe way.

MediaCopy 3000 reads and writes manifests in the
[ASC Media Hash List](mhl-format.md) format.
A manifest proves two things: The **integrity** of a copy (the bytes did not
change), and its **completeness** (every file is there).

Subsequent copies add their own manifest to the chain of the folder,
establishing its travel history

![The job queue](images/queue.png)
