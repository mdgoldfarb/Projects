function plate = initializePlate(n)
plate=zeros(n,n);
for i=1:n
    for j=1:n
        if i==1 && j==1
            plate(i,j)=50;
        end
        if i==1 && j~=1 && j~=n
            plate(i,j)=100;
        end 
        if i==1 && j==n
            plate(i,j)=87.5;
        end 
        if j==n && i~=1 && i~=n
            plate(i,j)=75;
        end 
        if i==n && j==n
            plate(i,j)=62.5;
        end 
        if i==n && j~=n && j~=1
            plate(i,j)=50;
        end 
        if i==n && j==1
            plate(i,j)=25;
        end 
        if i~=1 && i~=n && j==1
            plate(i,j)=0;
        end 
        if i~=1 && i~=n && j~=1 && j~=n
            plate(i,j)=25;
        end
    end
end

        
            