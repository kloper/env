#!/usr/bin/python
# -*- Python -*-

## \file
#
# \brief pylint wrapper from cygwin to native Windows python
#
# Copyright (c) 2014-2015 Cisco Corporation. All rights reserved.
#
# \page License
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# 1. Redistributions of source code must retain the above copyright notice,
#    this list of conditions and the following disclaimer.
#
# 2. Redistributions in binary form must reproduce the above copyright notice,
#    this list of conditions and the following disclaimer in the documentation
#    and/or other materials provided with the distribution.
#
# 3. The name of Cisco may not be used to endorse or promote products derived
#    from this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY CISCO "AS IS" AND ANY EXPRESS OR IMPLIED
# WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT ARE
# EXPRESSLY AND SPECIFICALLY DISCLAIMED. IN NO EVENT SHALL CISCO BE LIABLE FOR
# ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
# DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
# OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
# HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
# STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
# ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.
#

"""Wrapper for Cygwin Emacs to run Windows's python Pylint
"""

import os
import sys
import subprocess


def to_win_path(path):
    """Run cygpath to make Cygwin path to Windows path"""
    return subprocess.check_output(["cygpath", "-m", path]).rstrip()

def to_unix_path(path):
    """Run cygpath to make Windows path to Cygwin path"""
    return subprocess.check_output(["cygpath", "-u", path]).rstrip()

debug = False                       #pylint: disable=invalid-name
logname = "pylint-wrapper.log"     #pylint: disable=invalid-name

if debug:
    with open(logname, "w+") as log:
        log.write(" ".join(sys.argv) + "\n")

python3 = False

if python3:
    python_root = os.path.join("/cygdrive", "c", #pylint: disable=invalid-name
                               "WinPython-3.4.3.1",
                               "python-3.4.3.amd64")
else:
    python_root = os.path.join("/cygdrive", "c", #pylint: disable=invalid-name
                               "WinPython-2.7.9.5-32",
                               "python-2.7.9")

#pylint: disable=invalid-name
python_scripts_dir = os.path.join(python_root, "Scripts")

file_to_check = to_win_path(sys.argv[-1])

args = [os.path.join(python_root, "python"), "-m", "pylint"] + \
  sys.argv[1:-1] + \
  [file_to_check]

if debug:
    with open(logname, "a+") as log:
        log.write("--------------- spawn ----------------\n")
        log.write(" ".join(args) + "\n")

pylint = subprocess.Popen(
    args,
    stdout=subprocess.PIPE,
    stderr=subprocess.PIPE,
    cwd=python_root
)

(stdout, stderr) = pylint.communicate()

if debug:
    with open(logname, "a+") as log:
        log.write("----------- stdout ----------\n")
        log.write(stdout)
        log.write("----------- stderr ----------\n")
        log.write(stderr)

rc = pylint.wait()

cygpath = subprocess.Popen(
    ["cygpath", "-u", "-f", "-"],
    stdin=subprocess.PIPE,
    stdout=subprocess.PIPE,
    stderr=subprocess.PIPE,
    cwd=python_root
)

(cygpath_stdout, cygpath_stderr) = cygpath.communicate(stdout)

cygpath.wait()

sys.stdout.write(cygpath_stdout)
sys.stderr.write(stderr)
sys.stderr.write(cygpath_stderr)

exit(rc)

#
# pylint.py -- end of file
#
