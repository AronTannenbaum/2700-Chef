# Insert shebang here
use strict;
use warnings;

package Acme::Chef::Ingredient;

use strict;
use warnings;

use Carp;

use vars qw/$VERSION %Measures %MeasureTypes/;
$VERSION = '0.05';

%Measures = (
  ''          => '',

  g           => 'dry',
  kg          => 'dry',
  pinch       => 'dry',
  pinches     => 'dry',
  ml          => 'liquid',
  l           => 'liquid',
  dash        => 'liquid',
  dashes      => 'liquid',
  cup         => '',
  cups        => '',
  teaspoon    => '',
  teaspoons   => '',
  tablespoon  => '',
  tablespoons => '',
);

%MeasureTypes = (
  heaped => 'dry',
  level  => 'dry',
);

sub new {
   my $proto = shift;
   my $class = ref $proto || $proto;

   my $self = {};

   if ( ref $proto ) {
      %$self = %$proto;
   }

   my %args  = @_;

   %$self = (
     name         => '',
     value        => undef,
     measure      => '',
     measure_type => '',
     type         => '',
     %$self,
     %args,
   );

   bless $self => $class;

   $self->determine_type() if not $self->{type};

   return $self;
}


sub type {
   my $self = shift;

   $self->determine_type() if $self->{type} eq '';

   return $self->{type};
}


sub determine_type {
   my $self = shift;

   my $type = '';

   exists $Measures{$self->{measure}}
     or croak "Invalid measure specified: '$self->{measure}'.";

   $type = $Measures{ $self->{measure} };

   if ( exists $MeasureTypes{ $self->{measure_type} } ) {

      if ( $type eq '' ) {
         $type = $MeasureTypes{ $self->{measure_type} };
      } else {
         $MeasureTypes{ $self->{measure_type} } eq $type
           or croak "'Measure type' ($self->{measure_type}) does not match type of measure ($type).";
      }

   }

   $self->{type} = $type;
   return $self->{type};
}


sub value {
   my $self = shift;
   my $new_val = shift;

   $self->{value} = $new_val if defined $new_val;

   if (not defined $self->{value}) {
      my $name = $self->{name};
      croak "Attempted to use undefined ingredient '$name'.";
   }

   return $self->{value};
}


sub liquify {
   my $self = shift;

   $self->{type} = 'liquid';

   return $self;
}



package Acme::Chef::Container;

use strict;
use warnings;

use Carp;



use vars qw/$VERSION/;
$VERSION = '0.05';

sub new {
   my $proto = shift;
   my $class = ref $proto || $proto;

   my $self = {};

   if (ref $proto) {
      %$self = %$proto;
      $self->{contents} = [ map { $_->new() } @{$self -> {contents}} ];
   }

   %$self = (
     contents => [],
     %$self,
     @_,
   );

   return bless $self => $class;
}


sub put {
   my $self = shift;

   my @ingredients = @_;

   push @{$self->{contents}}, $_->new() for @ingredients;

   return $self;
}


sub fold {
   my $self = shift;

   my $ingredient = shift;

   croak "Invalid operation on empty container: fold."
     unless @{$self->{contents}};

   my $new_val = pop @{ $self->{contents} };

   $ingredient->value( $new_val->value() );

   return $ingredient;
}


sub add {
   my $self = shift;

   my $ingredient = shift;

   croak "Invalid operation on empty container: add."
     unless @{$self->{contents}};

   $self->{contents}->[-1]->value(
     $self->{contents}->[-1]->value() +
     $ingredient->value()
   );

   return $ingredient;
}


sub remove {
   my $self = shift;

   my $ingredient = shift;

   croak "Invalid operation on empty container: remove."
     unless @{$self->{contents}};

   $self->{contents}->[-1]->value(
     $self->{contents}->[-1]->value() -
     $ingredient->value()
   );

   return $ingredient;
}


sub combine {
   my $self = shift;

   my $ingredient = shift;

   croak "Invalid operation on empty container: combine."
     unless @{$self->{contents}};

   $self->{contents}->[-1]->value(
     $self->{contents}->[-1]->value() *
     $ingredient->value()
   );

   return $ingredient;
}


sub divide {
   my $self = shift;

   my $ingredient = shift;

   croak "Invalid operation on empty container: divide."
     unless @{$self->{contents}};

   $self->{contents}->[-1]->value(
     $self->{contents}->[-1]->value() /
     $ingredient->value()
   );

   return $ingredient;
}


sub put_sum {
   my $self = shift;

   my @ingredients = @_;

   my $sum = 0;
   $sum += $_->value() for @ingredients;

   my $ingredient = Acme::Chef::Ingredients->new(
     name    => '',
     value   => $sum,
     measure => '',
     type    => 'dry',
   );

   $self->put($ingredient);

   return $ingredient;
}


sub liquify_contents {
   my $self = shift;

   foreach my $ingredient (@{$self->{contents}}) {
      $ingredient->liquify();
   }

   return $self;
}


sub stir_time {
   my $self = shift;

   my $depth = shift;

   return $self unless scalar @{$self->{contents}};

   $depth = $#{$self->{contents}} if $depth > $#{$self->{contents}};

   my $top = pop @{ $self->{contents} };
   splice @{$self->{contents}}, (@{$self->{contents}}-$depth), 0, $top;

   return $self;
}


sub stir_ingredient {
   my $self = shift;

   my $ingredient = shift;

   $self->stir_time($ingredient->value());

   return $self;
}


sub mix {
   my $self = shift;

   _fisher_yates_shuffle( $self->{contents} );

   return $self;
}


sub clean {
   my $self = shift;

   @{$self->{contents}} = ();

   return $self;
}


sub pour {
   my $self = shift;

   return @{ $self->{contents} };
}


sub print {
   my $self = shift;

   my $string = '';

   foreach my $ingr ( reverse @{$self->{contents}} ) {
      if ($ingr->type() eq 'liquid') {
         $string .= chr( $ingr->value() );
      } else {
         $string .= ' '.$ingr->value();
      }
   }

   return $string;
}


# From the Perl FAQ: (NOT a method)
# fisher_yates_shuffle( \@array ) :
# generate a random permutation of @array in place
sub _fisher_yates_shuffle {
    my $array = shift;
    my $i;
    for ($i = @$array; --$i; ) {
        my $j = int rand ($i+1);
        @$array[$i,$j] = @$array[$j,$i];
    }
}


# Please read the pod documentation in this file for
# details on how to reach the author and copyright issues.

package Acme::Chef;

use 5.006;
use strict;
use warnings;

use Carp;

use vars qw/$VERSION/;
$VERSION = '0.05';





# constructor compile
# 
# Takes source code and compiles program from it.

sub compile {
   my $proto = shift;
   my $class = ref $proto || $proto;

   my $code = shift;
   defined $code or croak "compile takes one argument: a code string.";

   my $self = {};

   bless $self => $class;

   my @paragraphs = $self->_get_paragraphs( $code );
   my @recipes    = $self->_paragraphsToRecipes(\@paragraphs);

   $_->compile() foreach @recipes;

   $self->{start_recipe} = $recipes[0]->recipe_name();

   $self->{recipes} = {
                        map { ($_->recipe_name(), $_) } @recipes
                      };

   return $self;
}


# method execute
# 
# Takes no arguments. Returns the program's output.

sub execute {
   my $self = shift;

   my $start_recipe = $self->{recipes}->{ $self->{start_recipe} }->new();

   $start_recipe->execute($self->{recipes});

   return $start_recipe->output();   
}


# method dump
# 
# Takes one optional argument. If it equals 'autorun',
# dump returns a string that, when evaluated, executes
# the program and returns the output.
# If the argument does not equal 'autorun', a different
# string is returned that reconstructs the Acme::Chef
# object.

sub dump {
   my $self = shift;
   my $type = shift;
   $type = '' if not defined $type;

   local $@ = undef;
   require Data::Dumper;

   my $dumper = Data::Dumper->new([$self], ['self']);
   $dumper->Indent(0);
   $dumper->Purity(1);

   my $dump = $dumper->Dump();

   if ($type =~ /^autorun$/) {
      $dump = 'do{my ' . $dump . ' bless $self => "' . (__PACKAGE__) . '"; $self->execute();} ';
   } else {
      $dump = 'do{my ' . $dump . ' bless $self => "' . (__PACKAGE__) . '";} ';
   }

   return $dump;
}


# private function _get_paragraphs

sub _get_paragraphs {
   my $self = shift;

   my $string = shift;

   $string =~ s/^\s+//;
   $string =~ s/\s+$//;

   return split /\n{2,}/, $string;
}


# private function _paragraphsToRecipes
# 
# Constructs recipes from an array ref of paragraphs.

sub _paragraphsToRecipes {
   my $self = shift;

   my $paragraphs = shift;
   $paragraphs = shift if not defined $paragraphs or not ref $paragraphs;

   while (chomp @$paragraphs) {}

   my @recipes;

   my $paragraph_no = 0;
   while (@$paragraphs) {
      my $recipe_name = shift @$paragraphs;
      $paragraph_no++;
      $recipe_name =~ /^[ ]*([\-\w][\- \w]*)\.[ ]*$/
        or croak "Invalid recipe name specifier in paragraph no. $paragraph_no.";
      $recipe_name = lc($1);

      last unless @$paragraphs;
      my $comments = shift @$paragraphs;
      $paragraph_no++;
      my $ingredients;
      if ( $comments =~ /^[ ]*Ingredients\.[ ]*\n/ ) {
         $ingredients = $comments;
         $comments = '';
      } else {
         last unless @$paragraphs;
         $ingredients = shift @$paragraphs;
         $paragraph_no++;
      }

      last unless @$paragraphs;
      my $cooking_time = shift @$paragraphs;
      $paragraph_no++;
      my $temperature;

      if ($cooking_time =~ /^[ ]*Cooking time:[ ]*(\d+)(?: hours?| minutes?)\.[ ]*$/) {
         $cooking_time = $1;
         last unless @$paragraphs;
         $temperature = shift @$paragraphs;
         $paragraph_no++;
      } else {
         $temperature = $cooking_time;
         $cooking_time = '';
      }

      my $method;
      if ($temperature =~ /^[ ]*Pre-heat oven to (\d+) degrees Celsius(?: gas mark (\d+))?\.[ ]*$/) {
         $temperature = $1;
         $temperature .= ",$2" if defined $2;
         last unless @$paragraphs;
         $method = shift @$paragraphs;
         $paragraph_no++;
      } else {
         $method = $temperature;
         $temperature = '';
      }

      $method =~ /^[ ]*Method\.[ ]*\n/
        or croak "Invalid method specifier in paragraph no. $paragraph_no.";
      
      my $serves = '';
      if (@$paragraphs) {
         $serves = shift @$paragraphs;
         if ($serves =~ /^[ ]*Serves (\d+)\.[ ]*$/) {
            $serves = $1;
            $paragraph_no++;
         } else {
            unshift @$paragraphs, $serves;
            $serves = '';
         }
      }

      push @recipes, Acme::Chef::Recipe->new(
        name         => $recipe_name,
        comments     => $comments,
        ingredients  => $ingredients,
        cooking_time => $cooking_time,
        temperature  => $temperature,
        method       => $method,
        serves       => $serves,
      );
      
   }
   
   return @recipes;
}


1;


package Acme::Chef::Recipe;

use strict;
use warnings;

use Carp;




use vars qw/$VERSION %Grammars @GrammarOrder %Commands/;
$VERSION = '0.05';

@GrammarOrder = qw(
  take_from add_dry put fold add remove combine divide
  liquify_contents liquify stir_time stir_ingredient
  mix clean pour refrigerate set_aside serve_with
  until_verbed verb
);

{ # scope of grammar definition

   my $ord         = qr/([1-9]\d*)(?:st|nd|rd|th)/;
   my $ord_noncap  = qr/[1-9]\d*(?:st|nd|rd|th)/;
   my $ingr_noncap = qr/[\-\w][\- \w]*/;
   my $ingr        = qr/($ingr_noncap)/;
   my $verb        = qr/([\-\w]+)/;

   %Grammars = (

     put => sub {
        my $recipe = shift;
        local $_ = shift;
        my $regex;
        if (/ into (?:the )?(?:$ord )?mixing bowl$/) {
           $regex = qr/^Put (?:the )?$ingr into (?:the )?(?:$ord )?mixing bowl$/;
        } else {
           $regex = qr/^Put (?:the )?$ingr$/;
        }
        /$regex/ or return();
        $recipe->require_bowl($2||1);
        $recipe->require_ingredient($1, 'put');
        return 'put', $1, ($2||1);
     },

     take_from => sub {
        my $recipe = shift;
        local $_ = shift;
        /^Take $ingr from refridgerator$/ or return();
        $recipe->require_ingredient($1);
        return 'take_from', $1;
     },

     fold => sub {
        my $recipe = shift;
        local $_ = shift;
        /^Fold (?:the )?$ingr into (?:the )?(?:$ord )?mixing bowl$/ or return();
        $recipe->require_bowl($2||1);
        $recipe->require_ingredient($1, 'fold');
        return 'fold', $1, ($2||1);
     },

     add => sub {
        my $recipe = shift;
        local $_ = shift;
        my $regex;
        if (/ into (?:the )?(?:$ord )?mixing bowl$/) {
           $regex = qr/^Add (?:the )?$ingr into (?:the )?(?:$ord )?mixing bowl$/;
        } else {
           $regex = qr/^Add (?:the )?$ingr()$/;
        }
        /$regex/ or return();
        $recipe->require_bowl($2||1);
        $recipe->require_ingredient($1, 'add');
        return 'add', $1, ($2||1);
     },

     remove => sub {
        my $recipe = shift;
        local $_ = shift;
        my $regex;
        if (/ from (?:the )?(?:$ord )?mixing bowl$/) {
           $regex = qr/^Remove (?:the )?$ingr from (?:the )?(?:$ord )?mixing bowl$/;
        } else {
           $regex = qr/^Remove (?:the )?$ingr()$/;
        }
        /$regex/ or return();
        $recipe->require_bowl($2||1);
        $recipe->require_ingredient($1, 'remove');
        return 'remove', $1, ($2||1);
     },

     combine => sub {
        my $recipe = shift;
        local $_ = shift;
        my $regex;
        if (/ into (?:the )?(?:$ord )?mixing bowl$/) {
           $regex = qr/^Combine (?:the )?$ingr into (?:the )?(?:$ord )?mixing bowl$/;
        } else {
           $regex = qr/^Combine (?:the )?$ingr()$/;
        }
        /$regex/ or return();
        $recipe->require_bowl($2||1);
        $recipe->require_ingredient($1, 'combine');
        return 'combine', $1, ($2||1);
     },

     divide => sub {
        my $recipe = shift;
        local $_ = shift;
        my $regex;
        if (/ into (?:the )?(?:$ord )?mixing bowl$/) {
           $regex = qr/^Divide (?:the )?$ingr into (?:the )?(?:$ord )?mixing bowl$/;
        } else {
           $regex = qr/^Divide(?: the)?$ingr()$/;
        }
        /$regex/ or return();
        $recipe->require_bowl($2||1);
        $recipe->require_ingredient($1, 'divide');
        return 'divide', $1, ($2||1);
     },

     add_dry => sub {
        my $recipe = shift;
        local $_ = shift;
        /^Add (?:the )?dry ingredients(?: into (?:the )?(?:$ord )?mixing bowl)?$/ or return();
        $recipe->require_bowl($1||1);
        return 'add_dry', ($1||1);
     },

     liquify_contents => sub {
        my $recipe = shift;
        local $_ = shift;
        /^Liquify (?:the )?contents of (?:the )?(?:$ord )?mixing bowl$/ or return();
        $recipe->require_bowl($1||1);
        return 'liquify_contents', ($1||1);
     },

     liquify => sub {
        my $recipe = shift;
        local $_ = shift;
        /^Liquify (?:the )?$ingr$/ or return();
        $recipe->require_ingredient($1, 'liquify');
        return 'liquify', $1;
     },

     stir_time => sub {
        my $recipe = shift;
        local $_ = shift;
        /^Stir (?:(?:the )?(?:$ord )?mixing bowl )?for (\d+) minutes?$/ or return();
        $recipe->require_bowl($1||1);
        return 'stir_time', $2, ($1||1);
     },

     stir_ingredient => sub {
        my $recipe = shift;
        local $_ = shift;
        /^Stir $ingr into (?:the )?(?:$ord )?mixing bowl$/ or return();
        $recipe->require_bowl($2||1);
        $recipe->require_ingredient($1, 'stir_ingredient');
        return 'stir_ingredient', $1, ($2||1);
     },

     mix => sub {
        my $recipe = shift;
        local $_ = shift;
        /^Mix (?:the (?:$ord )?mixing bowl )well$/ or return();
        $recipe->require_bowl($1||1);
        return 'mix', ($1||1);
     },

     clean => sub {
        my $recipe = shift;
        local $_ = shift;
        /^Clean (?:the )?(?:$ord )?mixing bowl$/ or return();
        $recipe->require_bowl($1||1);
        return 'clean', ($1||1);
     },

     pour => sub {
        my $recipe = shift;
        local $_ = shift;
        /^Pour contents of (?:the )?((?:[1-9]\d*(?:st|nd|rd|th) )?)mixing bowl into (?:the )?((?:[1-9]\d*(?:st|nd|rd|th) )?)baking dish$/ or return();
        my $m = $1 || 1;
        my $b = $2 || 1;
        $m =~ s/\D//g;
        $b =~ s/\D//g;
        $recipe->require_bowl($m);
        $recipe->require_dish($b);
        return 'pour', $m, $b;
     },

     refrigerate => sub {
        my $recipe = shift;
        local $_ = shift;
        /^Refrigerate(?: for (\d+) hours?)?$/ or return();
        return 'refrigerate', (defined $1 ? $1 : 0);
     },

     set_aside => sub {
        my $recipe = shift;
        local $_ = shift;
        /^Set aside$/ or return();
        return 'set_aside';
     },

     serve_with => sub {
        my $recipe = shift;
        local $_ = shift;
        /^Serve with $ingr$/ or return();
        # $ingr is a recipe name here
        return 'serve_with', lc($1);
     },

     verb => sub {
        my $recipe = shift;
        local $_ = shift;
        /^$verb (?:the )?$ingr$/ or return();
        $recipe->require_ingredient($2, 'verb');
        return 'verb', lc($1), $2;
     },

     until_verbed => sub {
        my $recipe = shift;
        local $_ = shift;
        /^$verb ((?:(?:the )?$ingr_noncap )?)until ${verb}ed$/ or return();
        my $ing = (defined $2 ? $2 : '');
        my $verbed = $3;
        $verbed .= 'e' if not exists $recipe->{loops}{$verbed};
        $ing =~ s/^the //;
        $ing =~ s/ $//;
        $recipe->require_ingredient($ing, 'until_verbed') if $ing ne '';
        return 'until_verbed', $verbed, $ing;
     },

   );

}

%Commands = (
   put => sub {
      my $recipe = shift;
      my $data   = shift;
      $recipe -> {bowls} -> [$data -> [2] - 1]
        ->
           put( $recipe -> {ingredients} -> {$data -> [1]} );
      return 1;
   },
   
   take_from => sub {
      my $recipe = shift;
      my $data   = shift;
      local $/   = "\n";
      my $value;
      while (1) {
         $value  = <STDIN>;
         last if $value =~ /^\s*\.?\d+/;
      }
      $recipe -> {ingredients} -> {$data -> [1]}
              -> value($value+0);
   },

   fold => sub {
      my $recipe = shift;
      my $data   = shift;
      $recipe -> {bowls} -> [$data -> [2] - 1]
        ->
           fold( $recipe -> {ingredients} -> {$data -> [1]} );
      return 1;
   },

   add => sub {
      my $recipe = shift;
      my $data   = shift;
      $recipe -> {bowls} -> [$data -> [2] - 1]
        ->
           add( $recipe -> {ingredients} -> {$data -> [1]} );
      return 1;
   },

   remove => sub {
      my $recipe = shift;
      my $data   = shift;
      $recipe -> {bowls} -> [$data -> [2] - 1]
        ->
           remove( $recipe -> {ingredients} -> {$data -> [1]} );
      return 1;
   },

   combine => sub {
      my $recipe = shift;
      my $data   = shift;
      $recipe -> {bowls} -> [$data -> [2] - 1]
        ->
           combine( $recipe -> {ingredients} -> {$data -> [1]} );
      return 1;
   },

   divide => sub {
      my $recipe = shift;
      my $data   = shift;
      $recipe -> {bowls} -> [$data -> [2] - 1]
        ->
           divide( $recipe -> {ingredients} -> {$data -> [1]} );
      return 1;
   },

   add_dry => sub {
      my $recipe = shift;
      my $data   = shift;
      $recipe -> {bowls} -> [$data -> [1] - 1]
        ->
           put_sum(
                    grep { $_->type() eq 'dry' }
                         values %{ $recipe -> {ingredients} }
                  );
      return 1;
   },

   liquify => sub {
      my $recipe = shift;
      my $data   = shift;
      $recipe -> {ingredients} -> {$data -> [1]} -> liquify();
      return 1;
   },

   liquify_contents => sub {
      my $recipe = shift;
      my $data   = shift;
      $recipe -> {bowls} -> [$data -> [1] - 1] -> liquify_contents();
      return 1;
   },

   stir_time => sub {
      my $recipe = shift;
      my $data   = shift;
      $recipe -> {bowls} -> [$data -> [2] - 1]
        ->
           stir_time( $data -> [1] );
      return 1;
   },

   stir_ingredient => sub {
      my $recipe = shift;
      my $data   = shift;
      $recipe -> {bowls} -> [$data -> [2] - 1]
        ->
           stir_ingredient( $recipe -> {ingredients} -> {$data -> [1]} );
      return 1;
   },

   mix => sub {
      my $recipe = shift;
      my $data   = shift;
      $recipe -> {bowls} -> [$data -> [1] - 1] -> mix();
      return 1;
   },

   clean => sub {
      my $recipe = shift;
      my $data   = shift;
      $recipe -> {bowls} -> [$data -> [1] - 1] -> clean();
      return 1;
   },

   pour => sub {
      my $recipe = shift;
      my $data   = shift;
      my @stuff  = $recipe -> {bowls} -> [$data -> [1] - 1] -> pour();

      $recipe -> {dishes} -> [$data -> [2] - 1] -> put( $_ ) foreach @stuff;

      return 1;
   },

   refrigerate => sub {
      my $recipe = shift;
      my $data   = shift;
      my $serves = $recipe->{serves};
      my $hours  = $data->[1];
      $serves  ||= 0;
      $hours   ||= 0;
      $recipe->{serves} = $hours if $serves < $hours;
      return 'halt';
   },

   set_aside => sub {
      my $recipe = shift;
      my $data   = shift;

      return 'break';
   },

   serve_with => sub {
      my $recipe = shift;
      my $data   = shift;

      my $rec_recipe = $data->[1];

      return "recurse.$rec_recipe" ;
   },

   verb => sub {
      my $recipe = shift;
      my $data   = shift;

      my $verb = $data->[1];
      my $ingr = $data->[2];
      return "loop.$verb.$ingr";
   },

   until_verbed => sub {
      my $recipe = shift;
      my $data   = shift;

      my $verb = $data->[1];

      if ( exists $recipe->{ingredients}->{$data->[2]} ) {
         my $ingr = $recipe->{ingredients}->{$data->[2]};
         $ingr->value( $ingr->value() - 1 );
      }

      return "endloop.$verb";
   },

);


sub new {
   my $proto = shift;
   my $class = ref $proto || $proto;

   my $self = {};

   if (ref $proto) {
      %$self = %$proto;

      $self->{bowls}  = [ map { $_->new() } @{$self -> {bowls }} ];
      $self->{dishes} = [ map { $_->new() } @{$self -> {dishes}} ];
      $self->{loops}  = { map { ( $_, $self->{loops}{$_} ) }
                              keys %{$self->{loops}} };

      if ( $self->{compiled} ) {
         $self->{ingredients} = { map {
             (
              $_,
              $self -> {ingredients} -> {$_} -> new()
             )
           } keys %{ $self->{ingredients} }
         };
      }
   }

   my %args  = @_;

   %$self = (
     compiled     => 0,
     name         => '',
     comments     => '',
     ingredients  => '',
     cooking_time => '',
     temperature  => '',
     method       => '',
     serves       => '',
     output       => '',
     loops        => {},
     bowls        => [],
     dishes       => [],
     %$self,
     %args,
   );

   bless $self => $class;
   return $self;
}


sub execute {
   my $self    = shift;

   my $recipes = shift;

   $self->compile() unless $self->{compiled};

   my @loop_stack;

   my $max_pos = $#{$self->{method}};
   my $exec_pos = 0;
   while (1) {

      my $next_method = $self->{method}->[$exec_pos];
#      print ' ' x scalar(@loop_stack), join(',', @$next_method),"\n";

      my $return = $Commands{$next_method->[0]}->($self, $next_method);

      last if $return eq 'halt';

      if ( $return =~ /^recurse\.([\-\w][\-\w ]*)/ ) {
         exists $recipes->{$1}
           or croak "Invalid recipe '$1' specified for recursion.";

         my $clone       = $self->new();
         my $sous_recipe = $recipes->{$1}->new(
           bowls  => $clone->{bowls},
           dishes => $clone->{dishes},
         );

         my $sous_done   = $sous_recipe->execute( $recipes );

         $self->output( $sous_done->output() );

         $self -> {bowls} -> [0]
           -> put( $sous_done -> first_bowl() -> new() -> pour() );

      } elsif ( $return =~ /^loop\.([^\.]+)\.([^\.]+)/ ) {
         my $verb = $1;
         my $ingr = $2;

         push @loop_stack, $verb;

         if ( not $self -> {ingredients} -> {$ingr} -> value() ) {
            pop @loop_stack;
            $exec_pos = $self -> {loops} -> {$verb} -> {end};
         }

      } elsif ( $return =~ /^endloop\.([^\.]+)/ ) {
         my $verb = $1;

         $exec_pos = $self -> {loops} -> {$verb} -> {start} - 1;

      } elsif ( $return =~ /^break/ ) {
         my $verb = pop @loop_stack;
         $exec_pos = $self -> {loops} -> {$verb} -> {end};
      }

      $exec_pos++;
      last if $exec_pos > $max_pos;
   }

   if ( $self->{serves} ) {
      foreach my $serve ( 0..($self->{serves}-1) ) {
         last if $serve > $#{$self->{dishes}};
         my $string = $self->{dishes}->[$serve]->print();
         $self->{output} .= $string;
      }
   }

   return $self;
}


sub first_bowl {
   my $self = shift;
   return $self->{bowls}->[0];
}


sub require_ingredient {
   my $self = shift;
   my $ingredient = shift;
   my $sub = shift;

   (defined $ingredient and exists $self->{ingredients}{$ingredient})
     or croak "Unknown ingredient '".(defined$ingredient?$ingredient:'<undefined>').
              "' required for recipe '$self->{name}'".
              (defined $sub?" in '$sub'":'').".";

   return $self;
}


sub output {
   my $self = shift;

   $self->{output} .= shift if @_;

   return $self->{output};
}


sub require_bowl {
   my $self = shift;
   my $no   = shift;

   return if @{$self->{bowls}} >= $no;

   while (@{$self->{bowls}} < $no) {
      push @{$self->{bowls}}, Acme::Chef::Container->new();
   }

   return $self;
}

sub require_dish {
   my $self = shift;
   my $no   = shift;

   return if @{$self->{dishes}} >= $no;

   while (@{$self->{dishes}} < $no) {
      push @{$self->{dishes}}, Acme::Chef::Container->new();
   }

   return $self;
}

sub recipe_name {
   my $self = shift;

   $self->{name} = shift if @_;

   return $self->{name};
}


sub compile {
   my $self = shift;

   return 0 if $self->{compiled};

   my @ingredients = split /\n/, $self->{ingredients};

   shift @ingredients; # remove header line

   @ingredients or croak "Failed compiling recipe. No ingredients specified.";

   my %ingredients;
   my $ingredient_no = 0;

   foreach (@ingredients) {
      $ingredient_no++;

      my $value;
      if (s/^[ ]*(\d+)[ ]//) {
         $value = $1;
      } else {
         $value = undef;
      }

      my $measure_type = '';
      foreach my $type ( keys %Acme::Chef::Ingredient::MeasureTypes ) {
         if ( s/^\Q$type\E[ ]// ) {
            $measure_type = $type;
            last;
         }
      }

      my $measure = '';
      foreach my $meas ( keys %Acme::Chef::Ingredient::Measures ) {
         next if $meas eq '';

         if ( s/^\Q$meas\E[ ]// ) {
            $measure = $meas;
            last;
         }
      }

      /[ ]*([\-\w][\- \w]*)[ ]*$/
        or croak "Invalid ingredient specification (ingredient no. $ingredient_no, name).";

      my $ingredient_name = $1;

      my $ingredient = Acme::Chef::Ingredient->new(
        name         => $ingredient_name,
        value        => $value,
        measure      => $measure,
        measure_type => $measure_type,
      );

      $ingredients{$ingredient_name} = $ingredient;
   }

   $self->{ingredients} = \%ingredients;

   $self->{method} =~ s/\s+/ /g;

   my @steps = split /\s*\.\s*/, $self->{method};

   shift @steps; # remove "Method."

   my $step_no = 0;
   foreach my $step (@steps) {
      $step_no++;

      foreach my $grammar (@GrammarOrder) {
         my @res = $Grammars{$grammar}->($self, $step);
         @res or next;

         if ( $res[0] eq 'verb' ) {
            my $verb = $res[1];
            my $ingr = $res[2];

            $self->{loops}->{$verb} = {start => ($step_no-1), test => $ingr};
         } elsif ( $res[0] eq 'until_verbed' ) {
            my $verb = $res[1];
            exists $self->{loops}->{$verb}
              or croak "Loop end without loop start '$verb'.";

            $self->{loops}->{$verb}->{end} = $step_no - 1;
         }

         $step = [@res];
         last;
      }

      croak "Invalid method step (step no. $step_no): '$step'."
        if not ref $step eq 'ARRAY';
   }

   if ( grep { not exists $self->{loops}{$_}{end} } keys %{$self->{loops}} ) {
      croak "Not all loop starting points have matching ends.";
   }

   $self->{method} = \@steps;

   $self->{compiled} = 1;

   return $self;
}




print do{my $self = bless( {'recipes' => {'japh souffle' => bless( {'serves' => '1','cooking_time' => '','bowls' => [bless( {'contents' => []}, 'Acme::Chef::Container' )],'name' => 'japh souffle','dishes' => [bless( {'contents' => []}, 'Acme::Chef::Container' )],'output' => '','comments' => '','loops' => {},'compiled' => 1,'ingredients' => {'numbers' => bless( {'value' => '110','measure_type' => '','measure' => '','name' => 'numbers','type' => ''}, 'Acme::Chef::Ingredient' ),'voodoo puppets' => bless( {'value' => '67','measure_type' => '','measure' => '','name' => 'voodoo puppets','type' => ''}, 'Acme::Chef::Ingredient' ),'commas' => bless( {'value' => '97','measure_type' => '','measure' => '','name' => 'commas','type' => ''}, 'Acme::Chef::Ingredient' ),'urine' => bless( {'value' => '8','measure_type' => '','measure' => 'l','name' => 'urine','type' => 'liquid'}, 'Acme::Chef::Ingredient' ),'crackpipes' => bless( {'value' => '116','measure_type' => '','measure' => '','name' => 'crackpipes','type' => ''}, 'Acme::Chef::Ingredient' ),'acid' => bless( {'value' => '97','measure_type' => '','measure' => 'cups','name' => 'acid','type' => ''}, 'Acme::Chef::Ingredient' ),'onions' => bless( {'value' => '114','measure_type' => '','measure' => '','name' => 'onions','type' => ''}, 'Acme::Chef::Ingredient' ),'idiots' => bless( {'value' => '102','measure_type' => '','measure' => '','name' => 'idiots','type' => ''}, 'Acme::Chef::Ingredient' ),'dweebs' => bless( {'value' => '115','measure_type' => '','measure' => '','name' => 'dweebs','type' => ''}, 'Acme::Chef::Ingredient' ),'salt' => bless( {'value' => '107','measure_type' => '','measure' => 'kg','name' => 'salt','type' => 'dry'}, 'Acme::Chef::Ingredient' ),'bottles of beer' => bless( {'value' => '99','measure_type' => '','measure' => '','name' => 'bottles of beer','type' => ''}, 'Acme::Chef::Ingredient' ),'laptops' => bless( {'value' => '101','measure_type' => '','measure' => '','name' => 'laptops','type' => ''}, 'Acme::Chef::Ingredient' ),'mouses' => bless( {'value' => '80','measure_type' => '','measure' => '','name' => 'mouses','type' => ''}, 'Acme::Chef::Ingredient' ),'potatoes' => bless( {'value' => '44','measure_type' => '','measure' => '','name' => 'potatoes','type' => ''}, 'Acme::Chef::Ingredient' ),'megawatts' => bless( {'value' => '111','measure_type' => '','measure' => '','name' => 'megawatts','type' => ''}, 'Acme::Chef::Ingredient' ),'oil' => bless( {'value' => '72','measure_type' => '','measure' => 'l','name' => 'oil','type' => 'liquid'}, 'Acme::Chef::Ingredient' ),'pins' => bless( {'value' => '32','measure_type' => '','measure' => '','name' => 'pins','type' => ''}, 'Acme::Chef::Ingredient' ),'creeps' => bless( {'value' => '74','measure_type' => '','measure' => '','name' => 'creeps','type' => ''}, 'Acme::Chef::Ingredient' ),'keyboards' => bless( {'value' => '47','measure_type' => '','measure' => '','name' => 'keyboards','type' => ''}, 'Acme::Chef::Ingredient' ),'sheep' => bless( {'value' => '117','measure_type' => '','measure' => '','name' => 'sheep','type' => ''}, 'Acme::Chef::Ingredient' ),'flour' => bless( {'value' => '101','measure_type' => '','measure' => 'g','name' => 'flour','type' => 'dry'}, 'Acme::Chef::Ingredient' ),'pines' => bless( {'value' => '108','measure_type' => '','measure' => '','name' => 'pines','type' => ''}, 'Acme::Chef::Ingredient' ),'hackers' => bless( {'value' => '104','measure_type' => '','measure' => '','name' => 'hackers','type' => ''}, 'Acme::Chef::Ingredient' )},'method' => [['put','potatoes',1],['put','onions',1],['put','flour',1],['put','salt',1],['put','bottles of beer',1],['put','acid',1],['put','oil',1],['put','pins',1],['put','pines',1],['put','onions',1],['put','laptops',1],['put','mouses',1],['put','keyboards',1],['put','idiots',1],['put','flour',1],['put','hackers',1],['put','voodoo puppets',1],['put','pins',1],['put','onions',1],['put','flour',1],['put','hackers',1],['put','crackpipes',1],['put','megawatts',1],['put','numbers',1],['put','commas',1],['put','pins',1],['put','crackpipes',1],['put','dweebs',1],['put','sheep',1],['put','creeps',1],['liquify_contents',1],['pour',1,1]],'temperature' => ''}, 'Acme::Chef::Recipe' )},'start_recipe' => 'japh souffle'}, 'Acme::Chef' ); bless $self => "Acme::Chef"; $self->execute();} 