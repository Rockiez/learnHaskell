
-- 使用list集合实现length
length' xs = sum[1 | _ <- xs]

-- 声明Type类型注意事项
-- 1. list和tuple都需要在声明中完整描述。
-- 2. Type Variable 只能！且必须！作为TypeClass的变量，不能作为type的变量。
-- TV的含义可能就是表示这个变量不确定是哪个type，但可以肯定是这个typeclass下的。