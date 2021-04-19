function fish_mode_prompt
    if [ $fish_key_bindings = fish_vi_key_bindings ]
        switch $fish_bind_mode
            case default
                set_color -b green
            case insert
                set_color -b blue
            case visual
                set_color -b yellow
            case replace-one
                set_color -b magenta
        end
        echo -n " "
    end
end

function fish_prompt
    set -l last_status $status
    set -l cwd (prompt_pwd)

    if not test $last_status -eq 0
        set_color --bold white -b red
        echo -n ' ! '
        set_color normal
    end

    # Display current path
    set_color white -b 585858
    echo -n " $cwd "

    # Show git branch and dirty state
    set -l git_branch (command git symbolic-ref HEAD 2> /dev/null | sed -e 's|^refs/heads/||')
    set -l git_dirty (command git status -s --ignore-submodules=dirty 2> /dev/null)
    if test -n "$git_branch"
        if test -n "$git_dirty"
            set_color black -b yellow
            echo -n " $git_branch "
        else
            set_color black -b green
            echo -n " $git_branch "
        end
    end

    # Add a space
    set_color normal
    echo -n ' '
end

set -g fish_right_prompt
