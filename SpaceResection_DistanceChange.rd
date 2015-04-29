\name{SpaceResection_DistanceChange}
\alias{SpaceResection_DistanceChange}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
SpaceResection_DistanceChange(PhotoCoor, LandCoor, Intrinsic, EPS = 0.1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{PhotoCoor}{
%%     ~~Describe \code{PhotoCoor} here~~
}
  \item{LandCoor}{
%%     ~~Describe \code{LandCoor} here~~
}
  \item{Intrinsic}{
%%     ~~Describe \code{Intrinsic} here~~
}
  \item{EPS}{
%%     ~~Describe \code{EPS} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (PhotoCoor, LandCoor, Intrinsic, EPS = 0.1) 
{
    fx = Intrinsic[1, 1]
    fy = Intrinsic[2, 2]
    cx = Intrinsic[1, 3]
    cy = Intrinsic[2, 3]
    A = rep(1, 3)
    B = rep(1, 3)
    for (i in c(1:3)) {
        tanA = (PhotoCoor[i, 1] - cx)/fx
        tanB = (PhotoCoor[i, 2] - cy)/sqrt(fy^2 + (PhotoCoor[i, 
            1] - cx)^2)
        A[i] = atan(tanA)
        B[i] = atan(tanB)
    }
    cosPHI12 = sin(B[1]) * sin(B[2]) + cos(B[1]) * cos(B[2]) * 
        cos(A[2] - A[1])
    cosPHI23 = sin(B[2]) * sin(B[3]) + cos(B[2]) * cos(B[3]) * 
        cos(A[3] - A[2])
    cosPHI31 = sin(B[1]) * sin(B[3]) + cos(B[1]) * cos(B[3]) * 
        cos(A[1] - A[3])
    S12 = sqrt((LandCoor[1, 1] - LandCoor[2, 1])^2 + (LandCoor[1, 
        2] - LandCoor[2, 2])^2 + (LandCoor[1, 3] - LandCoor[2, 
        3])^2)
    S23 = sqrt((LandCoor[2, 1] - LandCoor[3, 1])^2 + (LandCoor[2, 
        2] - LandCoor[3, 2])^2 + (LandCoor[2, 3] - LandCoor[3, 
        3])^2)
    S31 = sqrt((LandCoor[1, 1] - LandCoor[3, 1])^2 + (LandCoor[1, 
        2] - LandCoor[3, 2])^2 + (LandCoor[1, 3] - LandCoor[3, 
        3])^2)
    F12 = 0.5/(1 - cosPHI12)
    F23 = 0.5/(1 - cosPHI23)
    F31 = 0.5/(1 - cosPHI31)
    G12 = cosPHI12 * F12
    G23 = cosPHI23 * F23
    G31 = cosPHI31 * F31
    A1 = matrix(c(1, -1, 1, 1, 1, -1, -1, 1, 1), 3, 3, byrow = 3)
    a = matrix(c(F12 * S12^2, F23 * S23^2, F31 * S31^2), 3, 1)
    init = x = A1 \%*\% a
    tolerance = 10000
    count = 0
    while (count < 100 && tolerance > EPS) {
        if (x[1, 1] < 0 || x[2, 1] < 0 || x[3, 1] < 0) 
            break
        s1 = sqrt(x[1, 1])
        s2 = sqrt(x[2, 1])
        s3 = sqrt(x[3, 1])
        b = matrix(c(-G12 * (s1 - s2)^2, -G23 * (s2 - s3)^2, 
            -G31 * (s3 - s1)^2), 3, 1)
        temp = init + A1 \%*\% b
        des = abs(x - temp)
        if (des[which.max(des)] < EPS) 
            break
        count = count + 1
        x = temp
    }
    return(list(A = A, B = B, Dis = as.vector(sqrt(x))))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
