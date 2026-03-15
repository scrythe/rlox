tango commit_name="":
  #!/usr/bin/env bash
  if [ -z "{{commit_name}}" ]; then
    RUSTFLAGS=-Awarnings cargo bench --quiet --bench=tango -- compare -s 100 -o
  else
    RUSTFLAGS=-Awarnings cargo bench --quiet --bench=tango -- compare -s 100 -o target/benchmarks/{{commit_name}}/tango
  fi

dhat commit_name="":
  #!/usr/bin/env bash
  if [ -z "{{commit_name}}" ]; then
    RUSTFLAGS=-Awarnings cargo bench --quiet --bench=dhat
  else
    target/benchmarks/{{commit_name}}/dhat --bench
  fi

divan commit_name="":
  #!/usr/bin/env bash
  if [ -z "{{commit_name}}" ]; then
    RUSTFLAGS=-Awarnings cargo bench --quiet --bench=divan
  else
    target/benchmarks/{{commit_name}}/divan --bench
  fi

export:
  #!/usr/bin/env bash
  commit_name=$(git rev-parse HEAD)
  cargo export target/benchmarks/$commit_name -- bench

