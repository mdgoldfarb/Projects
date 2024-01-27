function updatedPlate = updateTemperature(plate)
updatedPlate=zeros(size(plate,1),size(plate,2));
for i=1:size(plate,1)
    for j=1:size(plate,2)
        if i~=1 && j~=1 && i~=size(plate,1) && j~=size(plate,2)
            updatedPlate(i,j)= (plate(i-1,j)+plate(i,j-1)+plate(i,j+1)+plate(i+1,j))/4;
        end
        if i==1 || j==1 || i==size(plate,1) || j == size(plate,2)
            updatedPlate(i,j)=plate(i,j);
        end
    end 
end 
            
            