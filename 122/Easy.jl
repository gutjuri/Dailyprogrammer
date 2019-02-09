# https://www.reddit.com/r/dailyprogrammer/comments/1berjh/040113_challenge_122_easy_sum_them_digits/

function digitalRoot(n)
  n >= 10 ? digitalRoot(digitSum(n)) : n
end

function digitSum(n)
  sum = 0
  while n > 0
    sum += n%10
    n = div(n, 10)
  end
  sum
end
