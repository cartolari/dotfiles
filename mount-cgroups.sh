#!/bin/bash

mount -o remount,rw /sys/fs/cgroup/

mkdir -p /sys/fs/cgroup/systemd
if ! mountpoint /sys/fs/cgroup/systemd > /dev/null; then
  mount -t cgroup -o none,name=systemd cgroup /sys/fs/cgroup/systemd
fi

mount_cgroup() {
  mkdir -p "/sys/fs/cgroup/$1"

  if ! mountpoint "/sys/fs/cgroup/$1" > /dev/null; then
    mount -t cgroup cgroup -o "$1" "/sys/fs/cgroup/$1"
  fi
}

mount_cgroup cpu
mount_cgroup cpuacct
mount_cgroup net_cls
mount_cgroup net_prio
mount_cgroup perf_event
mount_cgroup memory
mount_cgroup blkio
mount_cgroup pids
