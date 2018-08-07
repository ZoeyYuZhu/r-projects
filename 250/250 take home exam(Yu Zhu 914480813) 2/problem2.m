
%Problem 2
filePath = '/Users/zhuguanghua/Downloads/Bibtex/Bibtex_data.txt';
[ft_mat, lbl_mat] = read_data(filePath);

A = bibtextrSplit(:,1);
B = bibtextstSplit(:,1);
ft_mat = ft_mat.';
X_train = ft_mat(A,:);
lbl_mat = lbl_mat.';
Y_train = lbl_mat(A,:);
X_test = ft_mat(B,:);
Y_test = lbl_mat(B,:);

X = X_train;
Y = Y_train;
lamda = 5;
yita = 0.0000001;
W = randn(1836, 50);
H = randn(159, 50);
F_ob = zeros(20,1);
for i=1:20
    W = GD_W(W, H, X, Y, lamda, yita);
    H = GD_H(W, H, X, Y, lamda, yita);
    F_ob(i) = 1/2*(norm((Y - X*W*H.'),'fro')^2) + lamda*(norm(W, 'fro')^2) +lamda*(norm(H, 'fro')^2);
end
plot(F_ob);

yt = X_train*W*H.';
[a,b] = size(Y_test);
[c,d] = sort(yt, 2, 'descend');
k = 5;%(or k = 1)
p = zeros(a, k);
index = d(:, 1:k);

for i = 1:a
    p(i,1:k)=Y_test(i, index(i,:));
end
pk=sum(p,2)/k;
p_k=sum(pk)/a;
disp(p_k);

function W = GD_W(W, H, X, Y, lamda, yita)
    W = W - yita*(X.'*(X*W*H.' - Y)*H + 2*lamda*W);
end

function H = GD_H(W, H, X, Y, lamda, yita)
    H = H - yita*((X*W*H.' - Y).'*X*W + 2*lamda*H);
end



