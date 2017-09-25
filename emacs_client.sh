#!/bin/bash
#filename: emacs_client.sh

#if [ `ps axu | grep "Emacs.app" | grep nw | wc -l` -eq 1 ]
# if [ `ps axu | grep "Emacs.app" | grep "daemon" | wc -l` -eq 1 ]
# then
#     echo "Ready."
# else
#     echo "Starting server."
#     /usr/local/bin/emacs --daemon
# fi
# emacsclient -c "$@"