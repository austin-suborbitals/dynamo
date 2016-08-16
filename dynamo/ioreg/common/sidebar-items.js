initSidebarItems({"enum":[["FunctionType","Internal representation of function-type classification."],["FunctionValueType","Representation of register function types."],["RegisterPermissions","Represents the acces permissions of a register. Guards against writes/reads to registers that do not support it."],["RegisterWidth","Represents the width of the register. Curretnly, only supports registers up to 32bits."]],"fn":[["offset_width_to_ty","Converts the offset+index information into the smallest primitive numeric type we can."],["reg_width_to_ty","Convert the width of the register to the representative ast::Ty (i.e. R8 -> u8)"]],"struct":[["IoRegFuncDef","Internal descriptor for a function definition."],["IoRegInfo","Internal representation used to build the ioreg definition."],["IoRegOffsetIndexInfo","Represents the offset from the segment, and the index into the register of a register-portion."],["IoRegOffsetInfo","Internal descriptor for an offset into a Segment."],["IoRegSegmentInfo","Internal representation of a Segment."]]});