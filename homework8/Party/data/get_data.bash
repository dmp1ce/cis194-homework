#!/bin/bash
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

wget -P "$DIR" http://www.seas.upenn.edu/~cis194/spring13/extras/08-IO/company.txt
