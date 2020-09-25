#!/usr/bin/perl -w
use strict;
while(<>){
    s/,/_/g;
    s/\t/,/g;
    s/NULL//g;
    print;
}
