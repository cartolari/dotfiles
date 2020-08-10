# dotfiles

## Chromebook Setup - Crouton

Run

```bash
bash bootstrap/crouton-host.sh
```

To setup crouton install it and run:

```bash
sudo crouton -t cli-extra,core,keyboard,touch,xiwi -r focal -m http://mirrors.digitalocean.com/ubuntu/
```

Inside the chroot:

```bash
bootstrap/crouton-chroot.sh
```

To update it:

```bash
sudo crouton -t cli-extra,core,kde,kde-desktop,keyboard,touch,xiwi -r focal -u
```
