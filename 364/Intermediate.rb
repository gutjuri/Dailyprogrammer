# https://www.reddit.com/r/dailyprogrammer/comments/8sjcl0/20180620_challenge_364_intermediate_the_ducci/

require 'set'

def ducci_step tuple
  tuple.each_with_index.map do |num, index|
    (num - tuple[(index + 1) % tuple.size]).abs
  end
end

def ducci_seq first_tuple
  next_in_sequence = first_tuple
  Enumerator.new do |yielder|
    while true
      yielder << next_in_sequence
      next_in_sequence = ducci_step next_in_sequence
    end
  end
end

def determine_ducci_steps first_tuple
  known_tuples = Set[Array.new(first_tuple.size ,0)]
  ducci_seq(first_tuple).take_while { |value| known_tuples.add? value }.size + 1
end

while (input_line = gets) != nil
  first_tuple = input_line.sub(/[()]/, '').split(', ').map(&:to_i)
  puts determine_ducci_steps first_tuple
end
