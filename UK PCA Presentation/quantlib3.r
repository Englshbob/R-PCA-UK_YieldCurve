Simple call with a flat curve
bond <- list(settlementDays=1,
             issueDate=as.Date("2004-11-30"),
             faceAmount=100,
             dayCounter='Thirty360',
             paymentConvention='Unadjusted')
schedule <- list(effectiveDate=as.Date("2004-11-30"),
                 maturityDate=as.Date("2008-11-30"),
                 period='Semiannual',
                 calendar='UnitedStates/GovernmentBond',
                 businessDayConvention='Unadjusted',
                 terminationDateConvention='Unadjusted',
                 dateGeneration='Forward',
                 endOfMonth=1)
calc=list(dayCounter='Actual360',
          compounding='Compounded',
          freq='Annual',
          durationType='Modified')
coupon.rate <- c(0.02875)

params <- list(tradeDate=as.Date('2002-2-15'),
               settleDate=as.Date('2002-2-19'),
               dt=.25,
               interpWhat="discount",
               interpHow="loglinear")
setEvaluationDate(as.Date("2004-11-22"))

discountCurve.flat <- DiscountCurve(params, list(flat=0.05))
FixedRateBond(bond,
              coupon.rate,
              schedule,
              calc,
              discountCurve=discountCurve.flat)


#Same bond with a discount curve constructed from market quotes
tsQuotes <- list(d1w  =0.0382,
                 d1m  =0.0372,
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
