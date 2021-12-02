import pandas as pd
df = pd.read_csv("cookie_cats.csv")
df.head()
#let's see how many players had gate_30 vs gate_40
df.groupby("version").count()
#44700 and 45489 (close)
# This command makes plots appear in the notebook
%matplotlib inline
plot_df = df.groupby("sum_gamerounds")["userid"].count()
ax = plot_df.head(n=100).plot(x = "sum_gamerounds", y = "userid", kind = "hist" )
ax.set_xlabel("Game Rounds")
ax.set_ylabel("User Count")
df['retention_1'].mean() #0.44
#i.e. 44% players were retained on day 1
df.groupby("version")['retention_1'].mean()
#gate_30 version 44.8%, gate_40 version 44.2%
#should we be confident in this difference
boot_1d = [] #creating a list with bootstrapped mean for each version
for i in range(50):
  boot_mean = df.sample(frac = 1, replace = True).groupby('version')['retention_1'].mean()
  boot_1d.append(boot_mean)
boot_1d = pd.DataFrame(boot_1d)   #converting the list into dataframe
boot_1d.plot(kind = 'kde')
boot_1d['diff'] = (boot_1d['gate_30']-boot_1d['gate_40'])/boot_1d['gate_40']*100
ax = boot_1d['diff'].plot(kind = 'kde')
ax.set_xlabel('% difference in means')
# Calculating the probability that 1-day retention is greater when the gate is at level 30
prob = (boot_1d['diff'] > 0).mean()
# Pretty printing the probability
print(prob)
# Calculating 7-day retention for both AB-groups
df.groupby('version')['retention_7'].mean()
# Creating a list with bootstrapped means for each AB-group
boot_7d = []
for i in range(500):
    boot_mean = df.sample(frac=1, replace=True).groupby('version')['retention_7'].mean()
    boot_7d.append(boot_mean)
    
# Transforming the list to a DataFrame
boot_7d = pd.DataFrame(boot_7d)

# Adding a column with the % difference between the two AB-groups
boot_7d['diff'] = (boot_7d['gate_30'] - boot_7d['gate_40']) /  boot_7d['gate_30'] * 100

# Ploting the bootstrap % difference
ax = boot_7d['diff'].plot(kind = 'kde')
ax.set_xlabel("% difference in means")

# Calculating the probability that 7-day retention is greater when the gate is at level 30
prob = (boot_7d['diff'] > 0).sum() / len(boot_7d)

# Pretty printing the probability
'{:.1%}'.format(prob)
# So, given the data and the bootstrap analysis
# Should we move the gate from level 30 to level 40 ?
move_to_level_40 = False # True or False ?
