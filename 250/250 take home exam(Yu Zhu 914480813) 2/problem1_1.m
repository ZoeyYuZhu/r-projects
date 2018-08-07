%problem 1.1
S_star = sprand(100, 100, 0.05);
%disp(S_star);
W_star = randn(100,10);
%disp(W_star);
H_star = randn(100,10);
%disp(H_star);
M = S_star + W_star * (H_star.');
%disp(M);


W = randn(100,10);
H = randn(100,10);
Z = W_star*H_star';

S = randn(100,100);
U = randn(100,100);
beta = 1;
yita = 0.01;
lamda = 10;
F_ob = zeros(100,1);
time = 0;

tStart = tic;
time = zeros(100,1);
for i = 1:100
    W = (beta*(M - S)- U)*H*inv(eye(10) + beta*H'*H);
    H = (beta*M' - beta*S' - U')*W*inv(eye(10) + beta*W'*W);
    S = ST_S(W, H,  S,M, U, beta, lamda);
    U = U + yita*(W*H.' + S - M);
    f_ob = calc_ob(W, H, S,lamda);
    F_ob(i) = f_ob;
    time(i) = toc(tStart);
end
toc;
plot(F_ob);
figure;
plot(time, F_ob);

% function W = GD_W(W, H, S, M, U, beta, yita)
%     W = W - yita*(W + beta*(W*H.' + S - M)*H + U*H);
% end

% function H = GD_H(W, H, S, M, U, beta, yita)
%     H = H - yita*(H + beta*(W*H.' + S - M).'*H + U.'*W);
% end
%     W = GD_W(W, H, S, M, U, beta, yita);
%     H = GD_H(W, H, S, M, U, beta, yita);
function S = ST_S(W, H, S, M, U, beta, lamda)
    if M - W*H.' - U/beta > lamda/beta 
    S = M - W*H.' - U/beta - lamda/beta;
    elseif M - W*H.' - U/beta < -lamda/beta 
            S = M - W*H.' - U/beta + lamda/beta;
    else
        S = 0;
    end
end


function f_ob = calc_ob(W, H,S, lamda)

f_ob = 0.5*(norm(W, 'fro')^2 + norm(H, 'fro')^2) + lamda*(sum(abs(S(:))));
end