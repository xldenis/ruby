begin 1 end

begin
  1
ensure
  2
end

begin
ensure
end

begin
else
ensure
end

begin 1 ; ensure 2 end

begin 1 ensure 2 end

begin
  1
rescue 2

end

begin
  1
else
  2
end

begin
rescue def a ; end
  2
end

begin
rescue 'A' => e
end

begin
rescue :a => e
end

begin
rescue 2, 4, 5
end

begin
rescue 2, 4, 5 => a
end

begin
rescue
ensure
end
