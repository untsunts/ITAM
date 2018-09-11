#!/usr/bin/perl -w
#
#------------------------------------------------------------------
#   
#     running a list of problems 
#     j-l morales  
#     ITAM   2015
#
#------------------------------------------------------------------
#
#
use FileHandle;
use IPC::Open2;

  $filename = "lista_catena10.txt";
  open LIST, $filename or die "Couldn't open file $filename\n";

#     Now read and process one line at a time
#------------------
  $pid = open2(*RESPONSE, *MATLAB , 'matlab -nojvm');
  $pid or die;
#------------------
while( <LIST> )
{
    chomp;
    if ( $_ eq "" ) { print "\n   *** End of the list *** \n"; last; }
#
    @names = split;
    $name = shift @names;
    print "  Processing $name  \n";
        
    print MATLAB &q(<<"    ENDMATLAB");
    : path(path,'/usr/local/bin');
    : PCS_local2 ('$name', 1000, 10e-7);
    : 
    : disp( 'Next problem' );
    :
    ENDMATLAB

    while( $got = <RESPONSE> ) {
	last if $got =~ /Next problem/;
    }
}    
close MATLAB or die $!;
close RESPONSE or die $!;
close LIST or die $!;

sub q{
    my $string  = $_[0];
    $string =~ s/^\s*://gm;
    #$string =~ s{ (.*)\*/\s*$ }{ sprintf "%-73s*/\n", $1 }gmex;
    return $string;
}
