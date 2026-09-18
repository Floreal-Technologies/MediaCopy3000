# Introduction

MediaCopy 3000 copies media and records what it copied. It is made for both
digital imaging technicians (DITs) on a set, and for hobbyists who wants to
transfer media in a safe way.

MediaCopy 3000 reads and writes the [ASC Media Hash List](mhl-format.md)
industry standard. A manifest in that format proves two things about a copy:
**integrity**, the bytes did not change, and **completeness**, every file is
there. Each job writes one manifest and adds it to the chain of the folder. That
chain is the history of your media files.

![The job queue](images/queue.png)
