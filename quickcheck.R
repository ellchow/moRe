
must.be <- function(desc, expr, test){
  function(env)
    delayedAssign(desc, test(expr), assign.env=env)
}

equal.to <- function(expected) { function(output) output == expected }

less.than <- function(expected) { function(output) output < expected }

greater.than <- function(expected) { function(output) output > expected }

less.than.or.equal.to <- function(expected) { function(output) output <= expected }

greater.than.or.equal.to <- function(expected) { function(output) output >= expected }

between <- function(lb,ub) { function(output) (output >= lb) & (output <= ub) }

contains <- function(x) { function(output) x %in% output }

matches <- function(pat, ...) { function(output) length(grep(pat, output, ...)) > 0 }

for.all <- function(desc, fun.to.test, gen, test, n = 50){
  # list(params expected test)
  lapply(gen(n),
         function(params) {
           invisible(params)
           must.be(paste(desc, paste(list(as.list(params)), collapse = ' '), sep = ': '),
                   do.call(fun.to.test, as.list(params)),
                   test(params))
         })
}

gen.ints <- function(n)
  as.integer(runif(n,-1e8,1e8))

qc <- function(group.desc, ...){
  env <- new.env()
  for(test in list(...)){
    if(is.list(test)){
      for(test2 in test)
        test2(env)
    }else
      test(env)
  }

  list(run = function(){
    test.descs <- ls(env)

    test.results <- lapply(test.descs, function(t) env[[t]])
    names(test.results) <- test.descs

    test.results
  })
}


##### try it out...
## qc('foo',
##    must.be('one plus one is two', (1 + 1), equal.to(2)),
##    must.be('three plus four is five', {3 + 4}, equal.to(5)),
##    for.all('doubling int input yields 2 times input',
##            function(a) {a * 2},
##            gen.ints,
##            function(x) equal.to(x * 2),
##            n = 3)
##    ) -> x
## print(x$run())
