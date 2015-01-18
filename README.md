shared-block-ring
=================

A simple on-disk fixed length queue in the style of the
Xen [shared-memory-ring](https://github.com/mirage/shared-memory-ring).
In particular the producer and consumer APIs allow clients to control
exactly when data is exposed to the consumer and removed from the queue.

Example usage
-------------

First create a "block device"-- any file will do:
```
dd if=/dev/zero of=test.raw bs=10240 count=1
```

Then start two shells, in one run:
```
./main.native produce
```
and in the other
```
./main.native consume
```

