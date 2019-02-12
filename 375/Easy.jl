# https://www.reddit.com/r/dailyprogrammer/comments/aphavc/20190211_challenge_375_easy_print_a_new_number_by/

function addOneToDigs(n)
  blog = floor(Int, log10(n))
  digs = (x -> x+1).([div(n % 10^(i+1), 10^i) for i in blog:-1:0])
  reduce((acc, x) -> acc * (x == 10 ? 100 : 10) + x, digs)
end
