#!/usr/bin/perl

use strict;
use warnings;

my %graph = ( 
   'foo' => 1,
   'bar' => 2,
   'a' => 3,
   'b' => 4,
   'c' => 5,
);

my $chosen;
foreach my $keys (keys %graph) {
   $chosen = $keys;
   if ($chosen eq 'foo') { 
      last;
   }
}

print "$chosen\n";
