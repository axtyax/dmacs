import argparse
import subprocess
import pathlib
import os
import shutil

# Tilemacs directory
t_dir = pathlib.Path(__file__).parent.absolute()

parser = argparse.ArgumentParser(description='Run emacs using the tilemacs configuration.')

parser.add_argument('--fresh', dest='fresh', default=False, action='store_true',
                    help='Clear all package data before running tilemacs')
parser.add_argument('--emacs', dest='emacs', metavar='emacs', type=str,
                    help='Location of emacs installation')
parser.add_argument('--file', dest='filename', metavar='file', type=str,
                    help='File to open')
args = parser.parse_args()


if args.fresh:
    shutil.rmtree(os.path.join(t_dir, "src/straight"))

# Start emacs with the tilemacs config, in a detached process
subprocess.Popen([args.emacs,
		  '--quick',
		  '--load',
                  ('%s') % args.filename])
