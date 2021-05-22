**An application for working with the Tinkoff Investing API.**

Simple trading bot with multiple strategies:
1. DivFlow - purchase of dividend shares two weeks before the closing date of the register and subsequent sale
2. Rebalance - rebalancing the portfolio while maintaining the specified weights
3. RSX - buying shares of promising companies with good fundamentals and high growth potential


### Example to use with CRON:

`
#### MON-FRI 10:01 MSK
1 7 * * 1-5 /home/apps/dragon/run.sh DivFlow MOEX
#### MON-FRI 11:01 MSK
1 8 * * 1-5 /home/apps/dragon/run.sh DivFlow DE
#### MON-FRI 17:31 MSK | 14 March 16:31, 7 Nov 17:31
31 13 * * 1-5 /home/apps/dragon/run.sh DivFlow US
#### MON 12:00 MSK
0 9 * * 1 /home/apps/dragon/run.sh Rebalance MOEX
#### MON 17:00 MSK
0 14 * * 1 /home/apps/dragon/run.sh RSX US

`

_ЗЫ. Если пригодилось: поставь звездочку - мне будет оч. приятно =)_