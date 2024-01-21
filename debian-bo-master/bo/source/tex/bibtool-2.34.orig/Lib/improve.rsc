%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% This file is part of BibTool.
%% It is distributed under the GNU General Public License.
%% See the file COPYING for details.
%% 
%% (c) 1995 Gerd Neugebauer
%% 
%% Net: gerd@uni-koblenz.de
%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Style Impovements.
%
% delete empty fields

rewrite.rule {"^\" *\"$"}
rewrite.rule {"^{ *}$"}

% correct page ranges

rewrite.rule {pages # "\([0-9]+\) *\(-\|---\) *\([0-9]+\)" = "\1--\3"}

% delete delimiters if the field is purely numerical

rewrite.rule {"^[\"{] *\([0-9]+\) *[\"}]$" "\1"}
