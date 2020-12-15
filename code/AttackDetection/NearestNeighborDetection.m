%% This function takes the vector of loads to be tested, and the initial 
% letter of the system in use, and it returns a 0 if the loads are normal 
% and 1 if the loads are attacked

function [detection_flag, total_number_of_groups,number_of_flagged_groups,group_with_highest_threshold_violation,threshold_of_group_with_highest_violation,value_of_group_with_highest_violation] = NearestNeighborDetection(P_load,system)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Nearest neighbor using built in Matlab functions %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Settings
%system = 'p';       % t for texas, p for polish
norm = 0;           % normalize load data, 1=yes, 0=no
p = 0.90;           % probability of being in training dataset
distance = 'euclidean';     % 'seuclidean' 'hamming' 'chebychev' 'jaccard'
k = 1;              % k = number of neighbors to look at
realTimeTesting = 1;% if 1, test P_loads against thresholds !this is what we need to use in OpenPA!
MDvsFA = 0;         % if 1, compute and plot the MD vs FA curve (ROC curve) by testing different thresholds
training = 1;       % 1: dont include in training the attacked hours, 
                    % 2: remove from attacks, those hours that are in training
grouping = 1;       % 0: do not group
                    % 1: do grouping, checking all the groups created 
                    % 2: do grouping, checking the biggest chebychev and
                    % then the subsequent one - not implemented yet - 
groupTypes = 2;     % 1 = starting from highest load... 2 = starting from highest, 
                    % do not create groups for loads which are already included in the previously created groups
ng = 35;            % number of load groups
db = 7;             % maximum distance of buses for grouping

%% Load selected system
if system == 'p'
    %%load loadPolish
    load weekPolishOpenPA_11-09-18
    load attackedLoadsPolishMultiple
    load resultsMultiple
    load graph_polish    
    % load polishBaseLoadsOpenPA % for testing with OpenPA base loads
    % load ZA
    mpc = case2383wp_revised;
    P_load = P_load';
    %attackLoads(mpc.bus(:,3)~=0,end+1) = ZA;
    
    normalLoads(mpc.bus(:,3)~=0,:) = wP;
    %normalLoads(mpc.bus(:,3)~=0,end+1) = polishBaseLoadsOpenPA; % for testing with OpenPA base loads
    if length(P_load) ~= length(mpc.bus(:,1))
       loadsRT = P_load';
    else
        loadsRT = P_load(mpc.bus(:,3)~=0)';
    end
else
    load loadsTexasYearNew
    load attackLoadsTexas15
    load results15
    load graph_texas
    mpc =ASU01_2;
    %mpc = ext2int(mpc);
    normalLoads = zeros(2000,8784);
    normalLoads(mpc.bus(:,3)~=0,:) = loadTexasYearNew;
    if length(P_load) ~= length(mpc.bus(:,1))
       
       loadsRT = P_load;
    else
        loadsRT(mpc.bus(:,3)~=0,:) = P_load;
    end
end



%% Index of load buses
ind = find(normalLoads(:,1)~=0);

%% Number of buses
n = length(ind);

%% Index of attacked hours
hoursAtt = results(results(:,4)>1,1);
indAtt = find(results(:,4)>1);

%% Normalize each load by its average
if norm == 1
    avg = mean(normalLoads,2);
    normalLoads = normalLoads./avg;
    attackLoads = attackLoads./avg;  
    loadsRT = loadsRT./avg(mpc.bus(:,3)~=0)';
    
end

%% Attacked buses
% for each attack, find the buses which have been attacked/modified
attBuses(1:n,1:length(hoursAtt)) = 0;
for i = 1:length(hoursAtt)
    tt = find(abs(attackLoads(:,indAtt(i))-normalLoads(:,hoursAtt(i)))>0.1);
    attBuses(tt,i) = 1;
end


%% Create groups of loads
if grouping == 1
    A = [mpc.branch(:,1), mpc.branch(:,2)];
    uniqueA = cellstr(unique(categorical(A), 'rows'));
    d = digraph(uniqueA(:,1), uniqueA(:,2));
    m = full(adjacency(d));
    g = graph(m|m',d.Nodes);
    nodes = table2struct(g.Nodes,'ToScalar',true);
    nodes = cellfun(@str2num,nodes.Name); 
    [temp inddist] = sort(nodes);
    d = distances(g);
    d = d(inddist,inddist);
    d = d(ind,ind);
    [temp indsize] = sort(mpc.bus(ind,3),'descend');
    
    if groupTypes == 1
        all = [];
        for i = 1:ng  
            %q = 0.05;
            %groups.(strcat('a',num2str(i)))= find(random('Binomial',1,q,length(ind),1)==1);
            groups.(strcat('a',num2str(i)))=  find(d(indsize(i),:)<db);
            all = [all groups.(strcat('a',num2str(i)))];
        end
    end
    
    if groupTypes == 2
        i = 1;
        groups.(strcat('a',num2str(i))) =  find(d(indsize(i),:)<db);
        all = groups.(strcat('a',num2str(1)));
        j = 2;
        while j <= ng  && i < length(d(:,1))
            i = i + 1;
            if ismember(indsize(i),all)==0
                groups.(strcat('a',num2str(j))) =  find(d(indsize(i),:)<db);
                all = [all groups.(strcat('a',num2str(j)))];
                j = j + 1;
            end                 
        end
        ng = j - 1;
    end
    
    % check if all buses are covered
    % not needed in OpenPA
%     check = ismember(1:length(d(:,1)), all);
%     temp = 1:length(d(:,1));
%     nl = 1:length(normalLoads(:,1));
%     nl(ind) = [];
%     H = plot(g,'Layout','force');        
%     highlight(H,inddist(nl),'MarkerSize',1)%,'NodeColor','r')
%     highlight(H,inddist(ind(temp(check==0))))%,'NodeColor','r')
%     for j = 1:ng           
%         highlight(H,inddist(ind(groups.(strcat('a',num2str(j))))),'NodeColor','r')
%         highlight(H,inddist(ind(groups.(strcat('a',num2str(j))))),'NodeColor',[0 0.4470 0.7410])
%     end
%     for i = 1:length(hoursAtt)           
%         highlight(H,inddist(find(attBuses(:,i)==1)),'NodeColor','r')
%         che = sum(check==0)
%         highlight(H,inddist(find(attBuses(:,i)==1)),'NodeColor',[0 0.4470 0.7410])
%     end
    
end




%% Create training and testing datasets
% Each hour is assigned to training with p probability and to testing with
% (1-p) probability
r = random('Binomial',1,p,1,length(normalLoads(1,:)));
%r = random('Binomial',1,p,1,length(normalLoads(1,1:167))); % for testing with OpenPA base loads

% not needed in OpenPA
if training == 1
    r(hoursAtt) = 1;  % exclude attacked hours from being in training
end

hist = normalLoads(ind,r==1)';
test = normalLoads(ind,r==0)';
%test(end+1,:) = normalLoads(ind,168); % for testing with OpenPA base loads
att = attackLoads(ind,results(:,4)>1)'; % load buses by succesfull attacks
%att(end+1,:) = attackLoads(ind,696);
hoursAtt = results(results(:,4)>1,1);

% not needed in OpenPA
if training == 2
% remove from "att" the attacks corresponding to hours which are in the
% training dataset
    rr = find(r==0);
    indAtt = ismember(hoursAtt,rr);
    att =att(indAtt,:);
end

if MDvsFA == 1

    %% Perform detection

    % with no groups
    if grouping == 0
        % Find distances
        [Idx,D] = knnsearch(hist,test,'K',k,'Distance',distance);
        [IdxA,DA] = knnsearch(hist,att,'K',k,'Distance',distance);

        Dm = mean(D,2);
        DAm = mean(DA,2);

        x=find(r==0)';
        x2=results(results(:,4)>1,1);
        x2 = 100;
        scatter(x,Dm);
        hold on;
        scatter(x2,DAm,'r');
        xlabel('Hour of the year')
        ylabel('Distance from closest neighbor')
        legend('normal','attacked')

    %     
    elseif grouping == 1
        % Find distances
        for i = 1:ng
           indtemp = groups.(strcat('a',num2str(i)));
           [Idx,D] = knnsearch(hist(:,indtemp),test(:,indtemp),'K',k,'Distance',distance);
           [IdxA,DA] = knnsearch(hist(:,indtemp),att(:,indtemp),'K',k,'Distance',distance);

           Dm(:,i) = D(:,k);
           DAm(:,i) = DA(:,k);

    %       x=find(r==0)';
    %       x2=results(results(:,4)>1,1);
    %       x2 = 100;
    %       figure
    %       scatter(x,Dm(:,i));
    %       hold on;
    %       scatter(x2,DAm(:,i),'r');
        end
        % Determine MD and FA
        for t = 1:101
            detA=zeros(length(att(:,1)),1);
            det=zeros(length(test(:,1)),1);
            for i = 1:ng
                th = max(Dm(:,i))*(0.8+t/500);
               % th = max(Dm(:,i))
                %th = max(Dm(:,i));
                for j = 1:length(att(:,1))
                    if DAm(j,i) > th;
                        detA(j) = detA(j) + 1;
                    end
               end  
               for j = 1:length(test(:,1))
                    if Dm(j,i) > th%prctile(Dm(:,i),th)
                        det(j) = det(j) + 1;
                    end
               end  
            end
            res = [sum(det==0) sum(detA==0) sum(detA==1)];
            md(t) = sum(detA==0)/length(att(:,1))*100;
            fa(t) = (length(test(:,1))-sum(det==0))/length(test(:,1))*100;
        end
        figure
        plot(fa,md,'-*')
        xlabel('False alarm %')
        ylabel('Missed detection %')
        grid on
        axis([0 10 0 inf])

%     elseif grouping == 2
%         for a = 1:length(att(:,1))       
%             diff = abs(bsxfun(@minus,hist,att(a,:)));
%             [IdxA,dista(a,1)] = knnsearch(hist,att(a,:),'K',1,'Distance','Chebychev');
%             ca(1) = find(diff(IdxA,:)==dista(a,1),1);
%             diff(:,ca) = [];
%             for x = 1:100            
%                 dista(a,1+x) = min(max(diff,[],2));
%                 [rr,cc] = find(diff==dista(a,1+x),1);
%                 ca(x+1) = cc;
%                 diff(:,ca(x+1)) = [];
%             end                  
%             disa = d(ca(1),ca);
%             finala(a) = mean(disa);
% 
%         end    
%         for n = 1:length(test(:,1))       
%             diffn = abs(bsxfun(@minus,hist,test(n,:)));
%             [Idx,distn(n,1)] = knnsearch(hist,test(n,:),'K',1,'Distance','Chebychev');
%             cn(1) = find(diffn(Idx,:)==distn(n,1),1);
%             diffn(:,cn) = [];
%             for x = 1:100            
%                 distn(n,1+x) = min(max(diffn,[],2));
%                 [rr,cc] = find(diffn==distn(n,1+x),1);
%                 cn(x+1) = cc;
%                 diffn(:,cn(x+1)) = [];
%             end     
%             disn = d(cn(1),cn);
%             finaln(n) = mean(disn);                 
%          end
    end
end

if realTimeTesting == 1
    
    % Find the minimum distance for the RT loads and compare it with the
    % maximum distance in thenormal loads. Do this for each group. If the
    % distance of the RT loads is higher than the normal ones, for any
    % group, use the flag to record it
    flag = 0;
    for i = 1:ng
           indtemp = groups.(strcat('a',num2str(i)));
           [Idx,D] = knnsearch(hist(:,indtemp),test(:,indtemp),'K',k,'Distance',distance);
           [IdxRT,DRT] = knnsearch(hist(:,indtemp),loadsRT(indtemp),'K',k,'Distance',distance);
           Dm(:,i) = D(:,k);
           DRTm(i) = DRT(:,k);
           Dout(i) = max(Dm(:,i));
           if DRTm(i) > max(Dm(:,i))
               flag = flag + 1;
           end
    end
    TH_out = [DRTm; Dout];
    TH_out(3,:) = (DRTm - Dout)./Dout*100;
    [value,position] = max(TH_out(3,:));
    
    total_number_of_groups = ng;
    number_of_flagged_groups = flag;
    group_with_highest_threshold_violation = position;
    threshold_of_group_with_highest_violation = TH_out(2,position);
    value_of_group_with_highest_violation = TH_out(1,position);
    
    if flag > 0
        detection_flag = 1;
    else
        detection_flag = 0;
    end
    
end
    
    