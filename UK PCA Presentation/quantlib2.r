Simple call with a flat curve
bond <- list(settlementDays=1,
             issueDate=as.Date("2007-03-10"),
             faceAmount=1000,
             dayCounter='ActualActual',
             paymentConvention='Unadjusted')
schedule <- list(effectiveDate=as.Date("2007-03-10"),
                 maturityDate=as.Date("2030-12-07"),
                 period='Semiannual',
                 calendar='UnitedKingdom/Settlement',
#                 calendar='UnitedStates/GovernmentBond',
                 businessDayConvention='Unadjusted',
                 terminationDateConvention='Unadjusted',
                 dateGeneration='Forward',
                 endOfMonth=1)
calc=list(dayCounter='ActualActual',
          compounding='Compounded',
          freq='Annual',
          durationType='Modified')
coupon.rate <- c(0.0475)

params <- list(tradeDate=as.Date('2020-06-25'),
               settleDate=as.Date('2020-06-25'),
               dt=.25,
               interpWhat="discount",
               interpHow="loglinear")
               setEvaluationDate(as.Date("2020-06-25"))

discountCurve.flat <- DiscountCurve(params, list(flat=0.001620))
FixedRateBond(bond,
              coupon.rate,
              schedule,
              calc,
              discountCurve=discountCurve.flat)


#Same bond with a discount curve constructed from market quotes
tsQuotes <- list(d1w  =0.0005,
                 d1m  =-0.0003,
                 fut1=96.2875,
                 fut2=96.7875,
                 fut3=96.9875,
                 fut4=96.6875,
                 fut5=96.4875,
                 fut6=96.3875,
                 fut7=96.2875,
                 fut8=96.0875,
                 s3y  =0.0398,
                 s5y  =0.0443,
                 s10y =0.05165,
                 s15y =0.055175)
tsQuotes <- list("flat" = 0.02)		## While discount curve code is buggy

discountCurve <- DiscountCurve(params, tsQuotes)
FixedRateBond(bond,
              coupon.rate,
              schedule,
              calc,
              discountCurve=discountCurve)

#Same bond calculated from yield rather than from the discount curve
yield <- 0.02
FixedRateBond(bond,
              coupon.rate,
              schedule,
              calc,
              yield=yield)


#same example with clean price
price <- 103.31
FixedRateBond(bond,
              coupon.rate,
              schedule,
              calc,
              price = price)

#example with default calc parameter
FixedRateBond(bond,
              coupon.rate,
              schedule,
              discountCurve=discountCurve)

#example with default calc and schedule parameters
schedule <- list(effectiveDate=as.Date("2004-11-30"),
                 maturityDate=as.Date("2008-11-30"))
FixedRateBond(bond,
              coupon.rate,
              schedule,
              discountCurve=discountCurve)

#example with default calc, schedule and bond parameters
FixedRateBond(,
              coupon.rate,
              schedule,
              discountCurve=discountCurve)

FixedRateBondPriceByYield(,0.0307, 100000, as.Date("2004-11-30"),
                          as.Date("2008-11-30"), 3, , c(0.02875),
                          , , , ,as.Date("2004-11-30"))

FixedRateBondYield(,90, 100000, as.Date("2004-11-30"), as.Date("2008-11-30"),
                   3, , c(0.02875), , , , ,as.Date("2004-11-30"))

lengths <- c(2.3,2.4,2.6,8,10,12,14,16,18,20,22,24,26,28,50)
coupons <- c( 0.0200, 0.0225, 0.0250, 0.0275, 0.0300,
              0.0325, 0.0350, 0.0375, 0.0400, 0.0425,
              0.0450, 0.0475, 0.0500, 0.0525, 0.0550 )
marketQuotes <- rep(100, length(lengths))
dateparams <- list(settlementDays=0, period="Annual", 
                   dayCounter="ActualActual", 
                   businessDayConvention ="Unadjusted")
curveparams <- list(method="ExponentialSplinesFitting", 
                    origDate = Sys.Date())
curve <- FittedBondCurve(curveparams, lengths, coupons, marketQuotes, dateparams)
if (require(zoo)) {
  z <- zoo(curve$table$zeroRates, order.by=curve$table$date)
  plot(z)
}
