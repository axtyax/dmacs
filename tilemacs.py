import argparse
import subprocess

parser = argparse.ArgumentParser(description='Run emacs using the tilemacs configuration.')

parser.add_argument('--emacs', dest='emacs', metavar='e', type=str,
                    help='Location of emacs installation')
args = parser.parse_args()

subprocess.Popen([args.emacs, '--quick', '--load', './src/init.el'])
