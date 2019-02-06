#  https://www.reddit.com/r/dailyprogrammer/comments/red6f/3262012_challenge_30_intermediate/

# helper function
function digSum(n, i)
  sum = 0
  while n > 0
    lastDigSum = n % 10
    lastDigSum = i % 2 == 0 ? lastDigSum : lastDigSum * 2
    if lastDigSum >= 10
      lastDigSum = lastDigSum % 10 + 1
    end
    sum += lastDigSum
    n = div(n, 10)
    i += 1
  end
  sum
end

# adds check digit

addCheckDigit(n) = n * 10 + (10 - rem(digSum(n, 1), 10))

# checks if number is valid

isValid(n) = rem(digSum(n, 0), 10) == 0