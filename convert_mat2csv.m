fileList = dir('*.mat');

for j = 1:length(fileList)
  load(fileList(j).name);
  csvwrite(sprintf('%s.csv', fileList(j).name(1:end-4)), eval(fileList(j).name(1:end-4)))
 
end

