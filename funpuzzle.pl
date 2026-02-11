use warnings;
use strict;
use feature 'say';

srand();

sub choose
{
    my @choices = @_;
    return $choices[int(rand(@choices))];
}

sub shuffle
{
    my @array = @_;
    for (my $i = $#array; $i > 0; $i--)
    {
        my $j = int(rand($i + 1));
        @array[$i, $j] = @array[$j, $i];
    }
    return @array;
}

sub rand_int
{
    my ($min, $max) = @_;
    return $min + int(rand($max - $min + 1));
}

sub tape_matches_at
{
    my ($tape, $pattern, $pos) = @_;

    if ($pos > length($tape) - length($pattern))
    {
        return 0;
    }

    return substr($tape, $pos, length($pattern)) eq $pattern ? 1 : 0;
}

sub find_matches
{
    my ($tape, $pattern) = @_;
    my @positions;

    for (my $i = 0; $i <= length($tape) - length($pattern); $i++)
    {
        if (tape_matches_at($tape, $pattern, $i))
        {
            push(@positions, $i);
        }
    }

    return @positions;
}

sub apply_rule_to_tape
{
    my ($tape, $rule, $pos) = @_;
    
    substr($tape, $pos, length($rule->{lhs})) = $rule->{rhs};
    return $tape;
}

sub make_inverse_rules
{
    my @rules = @_;
    my @inverse_rules;

    foreach my $rule (@rules)
    {
        push(@inverse_rules, { lhs => $rule->{rhs}, rhs => $rule->{lhs} });
    }

    return @inverse_rules;
}

sub generate_start_by_reversing
{
    my ($target, @rules) = @_;
    my @inverse_rules = make_inverse_rules(@rules);
    my $start = $target;

    my $steps = rand_int(20, 60);

    for (my $i = 0; $i < $steps; $i++)
    {
        my @moves;

        foreach my $rule (@inverse_rules)
        {
            my @matches = find_matches($start, $rule->{lhs});
            foreach my $pos (@matches)
            {
                push(@moves, { rule => $rule, pos => $pos });
            }
        }

        if (@moves == 0)
        {
            last;
        }

        my $move = choose(@moves);
        $start = apply_rule_to_tape($start, $move->{rule}, $move->{pos});
    }

    return $start;
}

sub pick_terminals
{
    my ($k) = @_;
    my @pool = ('a'..'h', 'm','n','p','q','r','s','t');
    @pool = shuffle(@pool);
    
    return @pool[0..$k-1];
}

sub pick_nonterminals
{
    my ($k, @avoid) = @_;
    my %avoid = map{$_ => 1} @avoid;
    my @pool = grep{!$avoid{$_}} ('A'..'Z');
    @pool = shuffle(@pool);
    
    return @pool[0..$k-1];
}

sub gen_fsa
{
    my @terminals = pick_terminals(2 + int(rand(2)));
    my ($a, $b, $c) = @terminals;
    my @nonterminals = pick_nonterminals(scalar(@terminals));

    my $family = rand_int(1, 3);
    my $L = rand_int(8, 18);

    my $goal = '';
    if ($family == 1)
    {
        my $k = rand_int(2, int($L/2));
        $goal = ($a x $k) . ($b x ($L - $k));
        $goal .= $c x rand_int(0, 3) if defined $c && rand() < 0.35;
    }
    elsif ($family == 2)
    {
        my $pairs = rand_int(3, 8);
        for (1 .. $pairs)
        {
            $goal .= $a . $b;
        }
        $goal .= $a if rand() < 0.35;
        $goal .= $b if rand() < 0.35;
        $goal .= $c x rand_int(0, 2) if defined $c && rand() < 0.25;
    }
    else
    {
        my $k1 = rand_int(1, 6);
        my $k2 = rand_int(2, 7);
        my $k3 = rand_int(1, 6);
        $goal = ($a x $k1);
        for (1 .. $k2)
        {
            $goal .= $b.$a;
        }
        $goal .= $b x $k3;
        $goal .= $c x rand_int(0, 2) if defined $c && rand() < 0.25;
    }

    my @rules;

    for (my $i = 0; $i < @terminals; $i++)
    {
        push(@rules, 
        {
            name => "$nonterminals[$i] -> $terminals[$i]", 
            lhs => $nonterminals[$i], 
            rhs => $terminals[$i] }
        );
    }

    for (my $i = 0; $i < @nonterminals; $i++)
    {
        for (my $j = $i + 1; $j < @nonterminals; $j++)
        {
            next if rand() < 0.25;
            push(@rules, 
            {
                name => "$nonterminals[$i]$nonterminals[$j] -> $nonterminals[$j]$nonterminals[$i]",
                lhs => $nonterminals[$i] . $nonterminals[$j],
                rhs => $nonterminals[$j] . $nonterminals[$i],
            });
        }
    }

    @rules = shuffle(@rules);
    return ("Friendly Symbol Arrangement", \@rules, $goal);
}

sub gen_cfg
{
    my @terminals = pick_terminals(2);
    my ($a, $b) = @terminals;
    my ($nonterminal) = pick_nonterminals(1, 'X');

    my $n = rand_int(3, 8);
    my $goal = ($a x $n) . ($b x $n);

    my @rules = 
    (
        { name => "$nonterminal -> $a$nonterminal$b", lhs => $nonterminal, rhs => $a . $nonterminal . $b },
        { name => "$nonterminal -> $a$b",  lhs => $nonterminal, rhs => $a . $b },
    );

    @rules = shuffle(@rules);
    return ("Carefree Fun Game", \@rules, $goal);

}

sub gen_csg
{
    my @terminals = pick_terminals(3);
    my ($a, $b, $c) = @terminals;
    my ($B, $C) = pick_nonterminals(2, 'X');

    my $n = rand_int(2, 6);
    my $goal = ($a x $n) . ($b x $n) . ($c x $n);

    my @rules = 
    (
        { name => "$B -> $b"."X",  lhs => $B, rhs => $b.'X' },
        { name => "X$B -> $B"."X", lhs => 'X'.$B,  rhs => $B.'X' },
        { name => "X$C -> $c",     lhs => 'X'.$C,  rhs => $c },
        { name => "X$c -> $c"."X", lhs => 'X'.$c,  rhs => $c.'X' },
    );

    @rules = shuffle(@rules);
    return ("Challenging Strategy Game", \@rules, $goal);
}

sub gen_mcsg
{
    my @terminals = pick_terminals(2);
    my ($a, $b) = @terminals;
    my ($A, $B) = pick_nonterminals(2, 'X', 'Y');

    my $n = rand_int(2, 7);
    my @chars = ($a x $n, $b x $n);
    @chars = shuffle(@chars);
    my $goal = join('', @chars);

    my @rules = 
    (
        { name => "$A -> $a"."X",  lhs => $A, rhs => $a.'X' },
        { name => "$B -> $b"."Y",  lhs => $B, rhs => $b.'Y' },
        { name => "XY -> YX",      lhs => 'X'.'Y', rhs => 'Y'.'X' },
        { name => "X$a -> $a"."X", lhs => 'X'.$a, rhs => $a.'X' },
        { name => "Y$b -> $b"."Y", lhs => 'Y'.$b, rhs => $b.'Y' },
        { name => "X$b -> $b",     lhs => 'X'.$b, rhs => $b },
        { name => "Y$a -> $a",     lhs => 'Y'.$a, rhs => $a },
    );

    @rules = shuffle(@rules);
    return ("Maliciously Complex Symbol Game", \@rules, $goal);
}

sub gen_chaos
{
    my @terminals = pick_terminals(2 + rand_int(0, 1));
    my ($A, $B, $C) = pick_nonterminals(3);

    my $goal_len = rand_int(6, 14);
    my $goal = join('', map{choose(@terminals)}(1..$goal_len));

    my @rules = 
    (
        { name => "$A -> $A$A",    lhs => $A,      rhs => $A.$A },
        { name => "$A$A -> $terminals[0]", lhs => $A.$A,   rhs => $terminals[0] },
        { name => "$B -> $terminals[1]",   lhs => $B,      rhs => $terminals[1] },
        { name => "$A$B -> $B$A",  lhs => $A.$B,   rhs => $B.$A },
    );
    if (defined $terminals[2])
    {
        push(@rules, { name => "$C -> $terminals[2]", lhs => $C, rhs => $terminals[2] });
        push(@rules, { name => "$B$C -> $C$B", lhs => $B.$C, rhs => $C.$B }) if rand() < 0.6;
    }

    @rules = shuffle(@rules);
    return ("Chaos", \@rules, $goal);
}

sub build_puzzle
{
    my ($mode) = @_;

    my ($mode_name, $rules_ref, $goal) = 
        $mode == 1 ? gen_fsa() :
        $mode == 2 ? gen_cfg() :
        $mode == 3 ? gen_csg() :
        $mode == 4 ? gen_mcsg() :
        gen_chaos();

    
    my $start = generate_start_by_reversing($goal, @$rules_ref);
    return ($mode_name, $rules_ref, $start, $goal);
}

# Start the game!

say "Fun Puzzle Playground";
say "Choose your own adventure:";
say "  1) Friendly Symbol Arrangement";
say "  2) Carefree Fun Game";
say "  3) Challenging Strategy Game";
say "  4) Maliciously Complex Symbol Game";
say "  5) Chaos!";
print "> ";

chomp(my $choice = <STDIN>);
$choice = 1 unless $choice =~ /^[1-5]$/;

my ($mode_name, $rules_ref, $start, $goal) = build_puzzle($choice);

my @tape = split(//, $start);
my @history = ();

sub tape_str { join('', @tape); }

sub is_goal { tape_str() eq $goal; }

sub show_tape
{
    my $tape_string = tape_str();
    say "\nMode:  $mode_name";
    say "Tape:  $tape_string";
    say "Goal:  $goal";
    say "Index: " . join('', map { $_ % 10 } (0..length($tape_string)-1));
}

sub show_rules
{
    say "\nRules / matches:";
    for (my $i = 0; $i < @$rules_ref; $i++) 
    {
        my $rule = $rules_ref->[$i];
        my @hits = find_matches(tape_str(), $rule->{lhs});
        my $hits = @hits ? join(',', @hits) : '-';
        printf("  [%d] %-22s at: %s\n", $i+1, $rule->{name}, $hits);
    }
}

sub reset_game
{
    @tape = split(//, $start);
    @history = ();
}

sub undo
{
    return unless @history;
    my $prev = pop @history;
    @tape = split(//, $prev);
}

sub new_puzzle_same_mode
{
    ($mode_name, $rules_ref, $start, $goal) = build_puzzle($choice);
    reset_game();
    say "\n--- New puzzle generated ---";
}

say "\nCommands:";
say "  <rule> <pos>   apply rule # at tape position";
say "  u              undo";
say "  r              reset to start";
say "  n              new randomized puzzle (same mode)";
say "  q              quit\n";

while (1)
{
    show_tape();
    show_rules();
    if (is_goal())
    {
        say "\n*** Hurray! You Won! ***\n";
        say "Type 'n' for a new puzzle, or 'q' to quit.";
    }

    print "\nEnter command: ";
    chomp(my $cmd = <STDIN>);
    $cmd =~ s/^\s+|\s+$//g;

    if ($cmd =~ /^(\d+)\s+(\d+)$/)
    {
        my ($rule_num, $pos) = ($1, $2);
        my $rule_idx = $rule_num - 1;

        if ($rule_idx >= 0 && $rule_idx < @$rules_ref)
        {
            my $rule = $rules_ref->[$rule_idx];
                        
            if (tape_matches_at(tape_str(), $rule->{lhs}, $pos))
            {
                push(@history, tape_str());
                @tape = split(//, apply_rule_to_tape(tape_str(), $rule, $pos));
            }
            else
            {
                say "No match for that rule at that position.";
            }
        }
        else
        {
            say "Invalid rule number.";
        }
    }
    elsif ($cmd eq 'u')
    {
        undo();
    }
    elsif ($cmd eq 'r')
    {
        reset_game();
    }
    elsif ($cmd eq 'n')
    {
        new_puzzle_same_mode();
    }
    elsif ($cmd eq 'q')
    {
        last;
    }
    elsif ($cmd eq '')
    {
        # do nothing on empty input
    }
    else
    {
        say "Whatchu talkin' 'bout? : Invalid command.";
    }
}

say "\nBye!";