# https://www.reddit.com/r/dailyprogrammer/comments/7j33iv/20171211_challenge_344_easy_baumsweet_sequence/

function baumsweet(n)
  n == 0 && return 1
  m = n
  while m & 3 == 0
    m = m >> 2
  end
  m & 1 == 0 ? 0 : baumsweet((m-1)>>1)
end

function baumsweetSeq(n)
  [baumsweet(i) for i in 1:n]
end

