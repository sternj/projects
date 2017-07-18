import java.util.Arrays;

public class FirstProj
{
	public static void main(String[] args)
	{
		long[] largest = new long[2];
		for(long i = 0; i < 1000000; i++)
		{
			int lnt = 0;
			long temp = i;
			do
			{
				temp = returnNextTerm(temp);
				lnt++;
			}while(temp>1);
			if(lnt > largest[1])
			{
				largest[0] = i;
				largest[1] = lnt;
			}
		}
		System.out.println(Arrays.toString(largest));
	}
	
	public static long returnNextTerm(long i)
	{
		
		if(i%2 == 0)
		{
			return i/2;
		}
		else
		{
			return (3*i)+1;
		}
	}
}
