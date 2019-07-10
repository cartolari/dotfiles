#!/bin/bash

read -r -s -p "Type the Crouton encryption passphrase " password
sudo start crouton-chroot CROUTON_PASSPHRASE "$password"
