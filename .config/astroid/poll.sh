#!/bin/bash
#
# Synchronize all configured accounts.

for account in $(basename ${HOME}/.config/mail/account.d/*)
do
	mail-sync fetch ${account} || exit $?
done
