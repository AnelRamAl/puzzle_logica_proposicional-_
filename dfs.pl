% Autor:
% Fecha: 15/02/2017

% [[1,0],[0,1],[-1,0],[0,-1]]
%Inicio = [[1,2,3],[8,0,6],[7,5,4]] Final = [[1,2,3],[8,0,4],[7,6,5]]
diag(Lista,Diag):-length(Lista,N),aux(Lista,N,Diag).
aux([],_,[]).
aux([H|B],N,[Ene|Resto]):-nesimo(H,N,Ene),Nt is N-1,aux(B,Nt,Resto).
nesimo([H|_],1,H).
nesimo([H|B],N,Elem):-Nt is N-1, nesimo(B,Nt,Elem).
goal([],_).
goal([H1|B1],[H2|B2]):-son_iguales(H1,H2),goal(B1,B2).
son_iguales([],_).
son_iguales([H|B1],[H|B2]):-son_iguales(B1,B2).
suma(N,N,N).
suma(I,N,Ts):-In is I+1,suma(In,N,Tsp),Ts is I+Tsp.
manh(Ni,Nf,N):-primj(Ni,Nf,0,N).
primj(_,_,9,0).
primj(Ni,Nf,I,N):-posx(Ni,I,X1,Y1),posx(Nf,I,X2,Y2),
    X is X1-X2, Y is Y1-Y2, abs(X,Xt),abs(Y,Yt),Nt is Xt+Yt,
    In is I+1,primj(Ni,Nf,In,Np),N is Np+Nt.
prin([]).
prin([H|B]):-writeln(H),prin(B).
                      %%    sc([[1,2,3,4],[5,0,6,7],[8,9,10,11],[12,13,14,15]],0,[[-1,0],[1,0],[0,-1],[0,1]],X,Y,RSus).
sc(Inicial,Cont,Mov,X,Y,ResultSus):- posx(Inicial,Cont,X,Y), suml(Mov,X,Y,M),          %%VA EN WHILE
                           scsr(Inicial,M,ResultSus).
                                      %%scsr([[1,2,3,4],[5,0,6,7],[8,9,10,11],[12,13,14,15]],[[1,2],[3,2],[2,1],[2,3]],X).

%suml([[-1,0],[1,0],[0,-1],[0,1]],2,2,M).
suml([],_,_,[]).
suml([[Xi,Yi]|Resto],X,Y,[[Xs,Ys]|Rest]):- Xs is Xi+X, Ys is Yi+Y, suml(Resto,X,Y,Rest).


scsr(_,[],[]).
scsr(Nodo,[[I,J]|B],[Sucesor|Resto]):-posx(Nodo,0,X,Y),posx(Nodo,Elem,I,J),
             pon(Nodo,I,J,0,Nodop),
             pon(Nodop,X,Y,Elem,Sucesor),
             scsr(Nodo,B,Resto).
pon([H|B],1,Y,Elem,[Hp|B]):-coloca(Elem,H,Y,Hp).
pon([H|B],X,Y,Elem,[H|Bp]):-Xp is X-1,pon(B,Xp,Y,Elem,Bp).
coloca(Elem,[_|B],1,[Elem|B]).
coloca(Elem,[P|B],Y,[P|Bp]):-Yp is Y-1,coloca(Elem,B,Yp,Bp).
valida([],[]).
valida([H|B],[H|Bt]):-ok(H),valida(B,Bt).
valida([_|B],L):-valida(B,L).
ok([X,Y]):-0<X,X<5,0<Y,Y<5.
mueve([],_,[]).
mueve([H|B],Cord,[Suma|Resto]):-suma(H,Cord,Suma),mueve(B,Cord,Resto).
suma([X1,Y1],[X2,Y2],[X3,Y3]):-X3 is X1+X2,Y3 is Y1+Y2.

                                  %%posx([[1,2,3,4],[5,0,6,7],[8,9,10,11],[12,13,14,15]],0,X,Y).
                                  % optiene las coordenadas del 0 -> 2,2
posx([R|_],Elem,1,Y):-member(Elem,R),posy(R,Elem,Y).
posx([_|B],Elem,X,Y):-posx(B,Elem,Xp,Y),X is Xp+1.
posy([Elem|_],Elem,1).
posy([_|B],Elem,Y):-posy(B,Elem,Z),Y is Z+1.


                     %%open([[1,2,3,4],[5,0,6,7],[8,9,10,11],[12,13,14,15]],[[1,2,3,4],[5,6,7,8],[9,10,11,12],[13,14,15,0]],0,[[-1,0],[1,0],[0,-1],[0,1]],X,Y,RSus,Rr).
unionDeWhile([],_,_,_,_,_,_,_,_,_,_).
unionDeWhile(Open,CurrNod,Closed,NewClosed,Goal, Cont,Mov,X,Y,Rsus,ROpenNew):-
        unionDe(Open,CurrNod,Closed,NewClosed,Goal, Cont,Mov,X,Y,Rsus,ROpenNew),
                   unionDeWhile(ROpenNew,_,NewClosed,_,Goal, Cont,Mov,_,_,_,_).
                          
         %unionDe([[[1, 2, 3, 4], [5, 0, 6, 7], [8, 9, 10, 11], [12, 13, 14, 15]]],_,[],_,[[1,2,3,4],[5,6,7,8],[9,10,11,12],[13,14,15,0]],0,[[-1,0],[1,0],[0,-1],[0,1]],_,_,Rsus,Ropen).
          %AQUI "unionDe" ARROJA UN FALSE.
          %%POR LO QUE AL APLICAR "unionDeWhile" TAMBIEN DA FALSE.
          %%CORRI "unionDe" PARA DOS PASOS QUE TENDRÍAN QUE PASAR DENTRO DEL WHILE y segun yo si dan los resultados correctos, pero
          %% por el false ya no puedo ingresarlo al "unionDeWhile".
          %%EN EL TRANCE EL ERROR DICE QUE ES PQ ENTRA A SCR DESPUES DE QUE YA CALCULO LOS RESULTADOS, PERO NO SÉ POR QUE.
          
          %OPEN TIENE EL PUZZLE INICIAL.
unionDe(Open,CurrNod,Closed,NewClosed,Goal, Cont,Mov,X,Y,Rsus,ROpenNew):-
                   copyFirst(Open,CurrNod), removeFirst(Open,OpenP),
                   push(CurrNod,Closed,NewClosed),                                    %actualizado Closed a Newclosed
            ifGoal(CurrNod, Goal, Cont,Mov,X,Y,Rsus,NewClosed,OpenP,ROpenNew).       %Cn is C +1,
                                                                                      % unionDe(Open,CurrNod,Closed,NewClosed,Goal, Cont,Mov,X,Y,Rsus,ROpenNew,Cn).

                          %current_node = remove-firts(open).
                          %insertfirst(current-node,closed).
                                        % FirstOpen =  current_node.
copyFirst([FirstOpen|_],FirstOpen).
removeFirst([_|Resto],Resto).
push(E, Es, [E|Es]).

                          %ifGoal([[1,2,3,4],[5,0,6,7],[8,9,10,11],[12,13,14,15]],[[1,2,3,4],[5,0,6,7],[8,9,10,11],[12,13,14,11]],0,[[-1,0],[1,0],[0,-1],[0,1]],X,Y,Rsus,[[1, 0, 3, 4], [5, 2, 6, 7], [8, 9, 10, 11], [12, 13, 14, 15]],[[1, 0, 3, 4], [5, 2, 6, 7], [8, 9, 10, 11], [12, 13, 14, 15]],R).
                          %%El resultado esta en la ultoma var
ifGoal(CurrNod, Goal, Cont,Mov,X,Y,Rsus,Closed,Open,CurrNod):- CurrNod == Goal.
ifGoal(CurrNod, Goal,Cont,Mov,X,Y,Rsus,Closed,Open,ROpenNew):- sc(CurrNod,Cont,Mov,X,Y,Rsus), forclose(Rsus,Closed,Open,_,ROpenNew).

    %R ingresa vacia"                       %forclose([[1,2],[3,4],[5,6]],[[7,8],[9,10],[5,6]],[[13,14],[15,16],[27,18]],_,M).
forclose([],_,R,_,R).      %El resultado esta en la ultoma var
forclose([Susci|Resto],Closed,Open,OpenNew,ROpenNew):-member(Susci,Closed), forclose(Resto,Closed,Open,OpenNew,ROpenNew).
forclose([Susci|Resto],Closed,Open,OpenNew,ROpenNew):-push(Susci,Open,OpenNew), forclose(Resto,Closed,OpenNew,_,ROpenNew).    %% member([[4,3],[4,5]],[[[4,3],[5,5]],[[1,2],[4,3],[5,7]]]).



