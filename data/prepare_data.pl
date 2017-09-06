use v5.14;
use warnings;
use strict;

my @files;
my $dir = '.';
opendir(DIR, $dir) or die $!;
while (my $file = readdir(DIR)) {
	# Use a regular expression to use only files *.TXT
        next if ($file !~ m/.*\.TXT$/);
	
	push(@files, $file);
}

@files = sort {$a cmp $b} @files;
foreach my $file (@files) {
	#~ next if($file eq $files[0]);
	
	open my $FILE, "<", $file or die $!;
	while (my $line = <$FILE>) {
		next if($line !~ m/^\d*?[A-Z]*?4\s.*/);
		print "$line\n";
	}
	close($FILE);
	
	#~ print "$file\n";
}

closedir(DIR);
exit 0;