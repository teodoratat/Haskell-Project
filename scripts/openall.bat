chdir ..
for /F "tokens=*" %%G in ('dir /a-D /S /B *.hs') do code %%G
chdir .\scripts