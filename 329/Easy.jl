# https://www.reddit.com/r/dailyprogrammer/comments/6wjscp/2017828_challenge_329_easy_nearest_lucky_numbers/

#using Pkg
#Pkg.add("BenchmarkTools")
using BenchmarkTools

function luckyNumbersTo(n::Number)
  A = [i for i in 1:2:n]
  i = 2
  while i <= length(A)
    k = A[i]
    deleteat!(A, k:k:length(A))
    i += 1
  end
  A
end

function challenge(n::Number)
  luckyNums = luckyNumbersTo(3*n)
  rng = searchsorted(luckyNums, n)
  if length(rng) == 0
    println("$(luckyNums[last(rng)]) < $n < $(luckyNums[first(rng)])")
  else
    println("$n is a lucky number")
  end
end

# The first 10_000_000 lucky numbers take about 8 seconds to compute
# The first 100_000 take about 3 ms
# The first 5_000 take about 50 μs
function benchmarkLuckyNums(n::Number)
  @benchmark luckyNumbersTo($n)
end
