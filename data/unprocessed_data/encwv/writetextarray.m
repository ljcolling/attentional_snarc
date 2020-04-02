
function success = writeTextArray(C,filename)
%success = writetextarray(C,filename)
fid = fopen(filename,'wt');

[M,N] = size(C);
for i=1:M-1
    for j=1:N-1
        bit = C{i,j};
        fprintf(fid, '%s\t',numIfstr(bit));
    end
    bit = C{i,N};
    fprintf(fid, '%s',numIfstr(bit));
    fprintf(fid, '\n');
end

for i=M
    for j=1:N-1
        bit = C{i,j};
        fprintf(fid, '%s\t',numIfstr(bit));
    end
    bit = C{i,N};
    fprintf(fid, '%s',numIfstr(bit));
end

fclose all;
success = 1;
end

function out = numIfstr(bit)
if isnumeric(bit) == 1
    out = num2str(bit);
else
    out = bit;
end
end

