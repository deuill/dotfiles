#!/bin/bash
#
# Synchronize all configured accounts.

for account in ${HOME}/.config/mail/*
do
	mail-sync fetch ${account} || exit $?
done
