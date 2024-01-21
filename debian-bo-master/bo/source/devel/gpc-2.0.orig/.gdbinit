define pr
set debug_rtx ($)
end

document pr
Print the full structure of the rtx that is $.
Works only when an inferior is executing.
end

define pt
set debug_tree ($)
end

document pt
Print the full structure of the tree that is $.
Works only when an inferior is executing.
end

define ptc
output (enum tree_code) $.common.code
echo \n
end

document ptc
Print the tree-code of the tree node that is $.
end

define pdn
output $.decl.name->identifier.pointer
echo \n
end

document pdn
Print the name of the decl-node that is $.
end

define prc
output (enum rtx_code) $.code
echo \ (
output $.mode
echo )\n
end

document prc
Print the rtx-code and machine mode of the rtx that is $.
end

define pi
print $.fld[0].rtx@7
end

document pi
Print the fields of an instruction that is $.
end

### Added by jtv@hut.fi

define ppi
print $.fld[1].rtx
pr
end

document ppi
Print the address of the insn preceding $.
end

define pni
print $.fld[2].rtx
pr
end

document pni
Print the address of the insn following $.
end

define dv
print $->list.value
echo \n
end

document dv
Proceed down through TREE_VALUE in nodes
end

define dp
print $->list.purpose
echo \n
end

document dp
Proceed down through TREE_PURPOSE in nodes
end

define dc
print $->common.chain
echo \n
end

document dc
Proceed down through TREE_CHAIN in nodes
end

define pti
output $->identifier.pointer
echo \n
end

document pti
Print the name of the current IDENTIFIER_NODE
end

define ptc
output (enum tree_code) (((int) $.common.code) & 0177)
echo \n
end

document ptc
Print the tree-code of the tree node that is $.
end

define pdn
output $.decl.name->identifier.pointer
echo \n
end

document pdn
Print the name of the decl-node that is $.
end

define ss
sym gpc1
source .gdbinit
echo \n
end

document pdn
Re-read .gdbinit file (to preset breakpoints after symbol command)
end

# Set some breakpoints

# for fancy_abort
#
b fancy_abort
b error

# Don't let abort actually run, as it will make
# stdio stop working and therefore the `pr' command below as well.
#
# Some hosts don't let you set breakpoint on abort and exit .gdbinit
# so I set it last.
b abort
