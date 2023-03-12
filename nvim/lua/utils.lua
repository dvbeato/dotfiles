local utils = {}

function utils.merge(tableA, tableB)
  for k, v in pairs(tableB) do
    if k and v then
      tableA[k] = v
    end
  end
  return tableA
end

function utils.concat(t1,t2)
    for i=1,#t2 do
        t1[#t1+1] = t2[i]
    end
    return t1
end

return utils
