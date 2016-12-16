#!/bin/bash
#
# Synchronize all configured accounts.

for account in $(basename ${HOME}/.config/mail/account.d/*)
do
	${HOME}/.local/bin/mail-sync fetch ${account} || exit $?
done
