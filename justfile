tango-export:
  cargo export target/benchmarks -- bench -- bench=tango

tango:
  cargo bench --bench=tango -- compare target/benchmarks/tango

divan:
  cargo bench --bench=divan

flamegraph:
  CARGO_PROFILE_RELEASE_DEBUG=true cargo flamegraph -- equality.lox

dhat:
  cargo run --features dhat-heap equality.lox
