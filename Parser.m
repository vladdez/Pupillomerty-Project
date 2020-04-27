clear all;
files = dir('*.asc');
for i=1:1
    filetoread = files(i).name;
    filetomake = ['2sub_', int2str(i), '.txt'];

FID = fopen(filetoread, 'r'); %open filetoread, call it "FID"
F2ID = fopen(filetomake, 'w'); %open filetomake, call it F2ID"
    
%the line below makes column headers in the output file
    
moreTrials = 1;
displayOn = 0;
currTrial = 0;
currepoch = 0;
pattern = 'DORN_';
erpattern = 'DORN_ERROR';
yessaccade = 0;
error = 0;
b = 0;
sound = '_';
circle = '_'
order = '_';
    %the main loop starts below, and ends at the end of the file (see matlab
    %book ch. 4)
    while moreTrials == 1 %as long as filerun is not null (i.e. 0) all the commands in this loop will keep looping
        line=fgetl(FID); %get the next line of data from FID, save it into a variable called "line"
        if line == -1 % if the end of the file has been reached, the fgetl function won't work anymore, so it will return -1.
            fclose(F2ID); %always close the file when you are done
            fclose(FID);
            moreTrials = 0; %leave the main loop
            break;
        end 

        %This is our cue to shift new trial.  reset all variables
        if findstr(line, 'start_trial')
            currTrial = currTrial +1;
            displayOn = 1;
            fprintf(F2ID, 'Trial\ttime\tx\ty\tpupil\tepoch\tsaccade\terror\tsound\torder\tcircle\n');
            %now look for gaze samples
            moreSamples = 1;
            currepoch = 0;
            error = 0;
            while moreSamples == 1 %as long as filerun is not null (i.e. 0) all the commands in this loop will keep looping
                line=fgetl(FID); %get the next line of data from FID, save it into a variable called "line"
                if strfind(line, pattern)
                    currepoch = currepoch +1;
                end
                if strfind(line, 'sound left')
                   sound = 'l';
                end
                if strfind(line, 'sound right')
                   sound = 'r';
                end
                
                if strfind(line, 'order seq')
                   order = 's';
                end
                if strfind(line, 'order par')
                   order = 'p';
                end
                if strfind(line, 'circle up')
                   circle = 'u';
                end
                if strfind(line, 'circle down')
                   circle = 'd';
                end
                if strfind(line, erpattern)
                    error = error +1;
                end
                
                if (b == 0) & isempty((strfind(line, 'SSACC')))& isempty((strfind(line, 'SFIX')))
                    yessaccade = 0;
                elseif (b == 1) & isempty((strfind(line, 'SSACC')))& isempty((strfind(line, 'SFIX')))
                    b = 0;
                    fprintf('0%d\n', currTrial);
                elseif strfind(line, 'SSACC')
                    yessaccade = 3;
                    b = 1;
                    fprintf('3%d\n', currTrial);
                elseif strfind(line, 'SFIX')
                    yessaccade = 4;
                    b = 1;
                    fprintf('4%d\n', currTrial);
               end
      
                if findstr(line, 'stop_trial')
                    moreSamples = 0;
                end  
                %if ~isempty(line)
                cols = textscan(line, '%d %f %f %f %f');
                if isempty(cols{1})
                    test = 1;%do nothing
                else
                    %    if isinteger(cols{1})
                    try
                        fprintf(F2ID, '%d\t%d\t%1.2f\t%1.2f\t%1.2f\t%d\t%d\t%d\t%c\t%c\t%c\n',...
                                        currTrial,cols{1}, cols{2}, cols{3}, cols{4}, currepoch, yessaccade, error, sound, order, circle);
                        
                    catch
                        test = 1;
                    end
                end

                    
            end
            
            
        end
    end
end
fprintf('done');
fclose('all');

