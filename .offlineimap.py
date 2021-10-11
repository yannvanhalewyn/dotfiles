#!/usr/bin/python
import re, os, sys

def get_authinfo_password(machine, login, port):
    s = "machine %s login %s port %s password ([^ \n]*)" % (machine, login, port)
    p = re.compile(s)
    authinfo = os.popen("gpg -q --no-tty -d ~/.authinfo.gpg").read()
    result = p.search(authinfo)
    if result is None:
        print("Could not find credentials for '%s'" % (login))
        sys.exit()
    return result.group(1)

def get_smtp_password(login):
    return get_authinfo_password("smtp.gmail.com", login, 587)
