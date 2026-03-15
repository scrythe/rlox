function __fish_just_complete_recipes
    if string match -rq '(-f|--justfile)\s*=?(?<justfile>[^\s]+)' -- (string split -- ' -- ' (commandline -pc))[1]
        set -fx JUST_JUSTFILE "$justfile"
    end
    set tokens (commandline -opc)
    if test (count $tokens) -ge 2
        switch $tokens[-1]
            case divan tango dhat
                find target/benchmarks -maxdepth 1 -mindepth 1 -type d \
                    | string replace target/benchmarks/ ""
                return
        end
    end
    printf "%s\n" (string split " " (just --summary))
end

complete -c just -n "__fish_seen_subcommand_from divan tango dhat" --no-files
